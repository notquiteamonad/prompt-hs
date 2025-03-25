{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- Module : System.Prompt
-- Description : A user-friendly, dependently-typed library for asking your users questions
-- License : BSD-3-Clause
--
-- @
-- data Colour = Red | Green | Blue deriving (Bounded, Enum, Eq, Show)
--
-- instance Chooseable Colour where
--  showChooseable = pack . show
--
-- main :: IO ()
-- main = do
--  name <- promptText Required RequireConfirmation $ pack "What is your name?"
--  favouriteColour <- promptChoice Optional DontConfirm (Proxy :: Proxy Colour) $ pack "And what is your favourite colour?"
--  putStrLn $
--    "Your name is " <> unpack name <> " and " <> case favouriteColour of
--      Just c -> "your favourite colour is " <> show c
--      Nothing -> "you didn't tell me your favourite colour."
-- @
module System.Prompt
  ( -- * Actions
    promptText,
    promptChoice,
    promptChoiceFromSet,

    -- * Data
    Chooseable (..),
    ChooseableItem (..),
    Requirement (..),
    PerRequirement,
    Confirmation (..),
  )
where

import Control.Monad (forM_)
import Control.Monad.IO.Class (MonadIO (liftIO))
import qualified Data.Char as C
import Data.Kind (Type)
import Data.List.NonEmpty (NonEmpty ((:|)), (<|))
import qualified Data.List.NonEmpty as NE
import Data.Proxy (Proxy (Proxy))
import Data.Text (Text)
import qualified Data.Text as T
import Lens.Micro (Lens', lens, (%~), (&), (.~))
import System.Exit (exitFailure)
import System.Prompt.Requirement
  ( Requirement (Optional, Required),
    SRequirement (SOptional, SRequired),
  )
import System.Terminal
  ( Direction (Downwards, Upwards),
    Event (KeyEvent),
    Interrupt (Interrupt),
    Key (ArrowKey, BackspaceKey, CharKey, EnterKey, EscapeKey),
    MonadColorPrinter (blue, cyan, foreground, magenta, yellow),
    MonadFormattingPrinter (bold, italic),
    MonadInput,
    MonadMarkupPrinter (Attribute, resetAttributes, setAttribute),
    MonadPrinter (flush, putText, putTextLn),
    MonadScreen (deleteLines, moveCursorUp),
    awaitEvent,
    ctrlKey,
    runTerminalT,
    withTerminal,
  )

-- | Things which can be chosen, following a prompt.
--
-- Most methods are automatically implemented for types with instances
-- of `Bounded` and `Enum`.
class Chooseable a where
  -- | Display the option in a user-friendly manner
  showChooseable :: a -> Text

  -- | Get the initially-selected value of `a`
  initialSelectionChooseable :: a
  default initialSelectionChooseable :: (Bounded a) => a
  initialSelectionChooseable = minBound

  -- | Get all values of `a`
  universeChooseableNE :: NonEmpty a
  default universeChooseableNE :: (Bounded a, Enum a) => NonEmpty a
  universeChooseableNE = minBound :| drop 1 [minBound .. maxBound]

instance (Chooseable a) => Chooseable (Maybe a) where
  showChooseable :: Maybe a -> Text
  showChooseable (Just a) = showChooseable a
  showChooseable Nothing = "Skip"

  initialSelectionChooseable :: Maybe a
  initialSelectionChooseable = Nothing

  universeChooseableNE :: NonEmpty (Maybe a)
  universeChooseableNE = Nothing <| (Just <$> universeChooseableNE)

-- | Things which can be chosen, but which are not part of a sum type.
class ChooseableItem a where
  -- | Display the option in a user-friendly manner
  chooseableItemText :: a -> Text

  -- | Get the initially-selected value of `a`
  initialSelectionChooseableItem :: a

instance (Chooseable a) => ChooseableItem a where
  chooseableItemText :: a -> Text
  chooseableItemText = showChooseable

  initialSelectionChooseableItem :: a
  initialSelectionChooseableItem = initialSelectionChooseable

-- | Returns `a` if the question is required, or `Maybe` `a` otherwise.
--
-- Represents what you'd expect to be the response to the given question, wrapped in `Maybe` for `Optional` questions.
type family PerRequirement (requirement :: SRequirement) (a :: Type) where
  PerRequirement 'SRequired a = a
  PerRequirement 'SOptional a = Maybe a

-- Whether to get confirmation from the user before accepting their answer.
data Confirmation
  = -- | Get confirmation
    RequireConfirmation
  | -- | Don't get confirmation
    DontConfirm

-- Represents instructions shown to the user while prompting them to make a choice
data ChoiceInstruction
  = ChoiceInstructionNormal
  | ChoiceInstructionNoOptionSelected

-- | Gets the text which will be displayed to the user for a given instruction.
instructionText :: ChoiceInstruction -> Text
instructionText = \case
  ChoiceInstructionNormal -> "You can type to search, or use the arrow keys. Press enter to select."
  ChoiceInstructionNoOptionSelected -> "No option selected. Try expanding your filter with backspace to see more options."

data PromptChoiceState (requirement :: SRequirement) (a :: Type) = PromptChoiceState
  { pcsConfirmation :: Confirmation,
    pcsFilter :: Text,
    pcsInstruction :: ChoiceInstruction,
    pcsOptions :: NonEmpty (PerRequirement requirement a),
    pcsFilteredOptions :: [PerRequirement requirement a],
    pcsSelectedOption :: Maybe (PerRequirement requirement a)
  }

_pcsFilter :: Lens' (PromptChoiceState requirement a) Text
_pcsFilter = lens pcsFilter \s x -> s {pcsFilter = x}

_pcsInstruction :: Lens' (PromptChoiceState requirement a) ChoiceInstruction
_pcsInstruction = lens pcsInstruction \s x -> s {pcsInstruction = x}

_pcsFilteredOptions :: Lens' (PromptChoiceState requirement a) [PerRequirement requirement a]
_pcsFilteredOptions = lens pcsFilteredOptions \s x -> s {pcsFilteredOptions = x}

_pcsSelectedOption :: Lens' (PromptChoiceState requirement a) (Maybe (PerRequirement requirement a))
_pcsSelectedOption = lens pcsSelectedOption \s x -> s {pcsSelectedOption = x}

data PromptTextState (requirement :: SRequirement) = PromptTextState
  { ptsConfirmation :: Confirmation,
    ptsCurrentInput :: Text,
    ptsPrompt :: Text,
    ptsRequirement :: Requirement requirement
  }

_ptsCurrentInput :: Lens' (PromptTextState requirement) Text
_ptsCurrentInput = lens ptsCurrentInput \s x -> s {ptsCurrentInput = x}

type Renderable m requirement a =
  ( ChooseableItem (PerRequirement requirement a),
    Eq (PerRequirement requirement a),
    MonadColorPrinter m,
    MonadFormattingPrinter m,
    MonadMarkupPrinter m,
    MonadScreen m
  )

-- | Ask the user to choose between the constructors of a sum type.
--
-- @
-- data Colour = Red | Green | Blue deriving (Bounded, Enum, Eq, Show)
--
-- instance Chooseable Colour where
--   showChooseable = Data.Text.pack . show
--
-- main :: IO ()
-- main = do
--   favouriteColour <- promptChoice Optional DontConfirm (Proxy :: Proxy Colour) $ "What is your favourite colour?"
--   print favouriteColour
-- @
promptChoice ::
  (result ~ PerRequirement requirement a, Chooseable result, Eq result, MonadIO m) =>
  -- | May they skip the question?
  Requirement requirement ->
  -- | Should they be asked to confirm their answer after selecting it?
  Confirmation ->
  -- | Proxy of type you want to get back
  Proxy a ->
  -- | What should the prompt say? (You need not add a space to the end of this text; one will be added)
  Text ->
  m result
promptChoice requirement confirmation pxy prompt =
  liftIO . withTerminal . runTerminalT $
    promptChoiceInternal pxy prompt (initialPromptChoiceState requirement confirmation)

-- | Ask the user to choose between arbitrary options.
--
-- Note: duplicate options will be removed.
promptChoiceFromSet ::
  (ChooseableItem result, Eq result, MonadIO m) =>
  -- | Should they be asked to confirm their answer after selecting it?
  Confirmation ->
  -- | The items from which they can choose (if you don't require an answer, add a "Skip" option)
  NonEmpty result ->
  -- | What should the prompt say? (You need not add a space to the end of this text; one will be added)
  Text ->
  m result
promptChoiceFromSet confirmation set prompt =
  liftIO . withTerminal . runTerminalT $
    promptChoiceInternal Proxy prompt (initialPromptChoiceStateFromSet confirmation $ NE.nub set)

-- | Ask the user to enter text.
--
-- The answer is stripped of leading and trailing whitespace.
--
-- > promptText Required DontConfirm "What is your name?"
--
-- > promptText Optional RequireConfirmation "What is your greatest fear?"
promptText ::
  (result ~ PerRequirement requirement Text, MonadIO m) =>
  -- | May they skip the question?
  Requirement requirement ->
  -- | Should they be asked to confirm their answer after entering it?
  Confirmation ->
  -- | What should the prompt say? (You need not add a space to the end of this text; one will be added)
  Text ->
  m result
promptText requirement confirmation prompt =
  liftIO . withTerminal . runTerminalT $
    promptTextInternal
      PromptTextState
        { ptsConfirmation = confirmation,
          ptsCurrentInput = "",
          ptsPrompt = prompt,
          ptsRequirement = requirement
        }

initialPromptChoiceState ::
  (Chooseable (PerRequirement requirement a)) =>
  Requirement requirement ->
  Confirmation ->
  PromptChoiceState requirement a
initialPromptChoiceState _ confirmation =
  PromptChoiceState
    { pcsConfirmation = confirmation,
      pcsFilter = "",
      pcsInstruction = ChoiceInstructionNormal,
      pcsOptions = universeChooseableNE,
      pcsFilteredOptions = NE.toList universeChooseableNE,
      pcsSelectedOption = Just initialSelectionChooseable
    }

initialPromptChoiceStateFromSet ::
  (ChooseableItem a) =>
  Confirmation ->
  NonEmpty a ->
  PromptChoiceState 'SRequired a
initialPromptChoiceStateFromSet confirmation options =
  PromptChoiceState
    { pcsConfirmation = confirmation,
      pcsFilter = "",
      pcsInstruction = ChoiceInstructionNormal,
      pcsOptions = options,
      pcsFilteredOptions = NE.toList options,
      pcsSelectedOption = Just initialSelectionChooseableItem
    }

promptChoiceInternal ::
  (MonadInput m, Renderable m requirement a) =>
  Proxy a ->
  Text ->
  PromptChoiceState requirement a ->
  m (PerRequirement requirement a)
promptChoiceInternal _ prompt s = do
  linesRendered <- renderPromptChoicePrompt prompt s
  awaitEvent >>= handlePromptChoiceEvent prompt linesRendered s

promptTextInternal ::
  (MonadColorPrinter m, MonadFormattingPrinter m, MonadInput m, MonadScreen m) =>
  PromptTextState requirement ->
  m (PerRequirement requirement Text)
promptTextInternal s = do
  renderPrompt $ ptsPrompt s
  withAttributes [foreground yellow] . putText $ ptsCurrentInput s
  flush
  awaitEvent >>= handlePromptTextEvent s

-- | Renders the prompt for the user and returns the number of lines output
renderPromptChoicePrompt ::
  (Renderable m requirement a) =>
  Text ->
  PromptChoiceState requirement a ->
  m Int
renderPromptChoicePrompt prompt s = do
  renderPromptLine prompt
  linesRendered <- renderOptionLines s
  renderSummaryLine s linesRendered
  renderInstructionLine
  renderFilterLine
  pure $ linesRendered + 4
  where
    renderInstructionLine = withAttributes [italic] . putTextLn . instructionText $ pcsInstruction s
    renderFilterLine = do
      withAttributes [bold, foreground magenta] $ putText "> "
      putText $ pcsFilter s
      flush

renderPrompt :: (MonadColorPrinter m, MonadFormattingPrinter m) => Text -> m ()
renderPrompt = withAttributes [bold, foreground blue] . putText . (<> " ")

renderPromptLine :: (MonadColorPrinter m, MonadFormattingPrinter m) => Text -> m ()
renderPromptLine = withAttributes [bold, foreground blue] . putTextLn

renderSummaryLine :: (Renderable m requirement a) => PromptChoiceState requirement a -> Int -> m ()
renderSummaryLine s numberOfOptionsRendered = do
  let nAll = length $ pcsOptions s
      nFiltered = length $ pcsFilteredOptions s
  withAttributes [italic, foreground cyan] . putTextLn $
    T.pack (show nFiltered)
      <> "/"
      <> T.pack (show nAll)
      <> " included by filter, "
      <> T.pack (show numberOfOptionsRendered)
      <> " shown."

renderOptionLines ::
  forall requirement a m.
  (Renderable m requirement a) =>
  PromptChoiceState requirement a ->
  m Int
renderOptionLines s = do
  let (selectedOption, otherOptions) = getVisibleOptions s
  case selectedOption of
    Just o -> withAttributes [bold, foreground yellow] . putTextLn . ("* " <>) $ chooseableItemText o
    Nothing -> pure ()
  forM_ otherOptions (withAttributes [foreground yellow] . putTextLn . ("  " <>) . chooseableItemText)
  pure $ length otherOptions + maybe 0 (const 1) selectedOption

-- | Returns a maximum of 6 options, including the one currently selected as the first option.
getVisibleOptions ::
  forall requirement a result.
  (result ~ PerRequirement requirement a, Eq result) =>
  PromptChoiceState requirement a ->
  (Maybe result, [result])
getVisibleOptions s = case pcsSelectedOption s of
  Just o | o `elem` filteredOptions -> (Just o, take 5 $ filter (/= o) filteredOptions)
  _ -> (Nothing, take 6 filteredOptions)
  where
    maxNumberOfOptions :: Int
    maxNumberOfOptions = length $ pcsFilteredOptions s
    filteredOptions :: [result]
    filteredOptions = take maxNumberOfOptions . dropWhile isNotSelectedOption . cycle $ pcsFilteredOptions s
    isNotSelectedOption :: result -> Bool
    isNotSelectedOption o = case pcsSelectedOption s of
      Just target -> o /= target
      Nothing -> False

handlePromptChoiceEvent ::
  forall m requirement a.
  (Renderable m requirement a, MonadInput m) =>
  Text ->
  Int ->
  PromptChoiceState requirement a ->
  Either Interrupt Event ->
  m (PerRequirement requirement a)
handlePromptChoiceEvent prompt linesRendered s e = do
  moveCursorUp (linesRendered - 1)
  deleteLines linesRendered
  case e of
    Left Interrupt -> liftIO exitFailure
    Right (KeyEvent key modifiers) -> case key of
      ArrowKey direction -> case direction of
        Upwards ->
          goAgain $
            s
              { pcsSelectedOption =
                  headViaNonEmpty
                    $ drop 1
                      . dropWhile (maybe (const False) (/=) (pcsSelectedOption s))
                      . cycle
                      . reverse
                    $ pcsFilteredOptions s
              }
        Downwards -> goAgain $ s {pcsSelectedOption = headViaNonEmpty . snd $ getVisibleOptions s}
        _ -> goAgain s
      BackspaceKey ->
        goAgain $
          s
            & _pcsFilter %~ T.dropEnd 1
            & applyFilter
            & _pcsInstruction %~ \case
              ChoiceInstructionNoOptionSelected -> ChoiceInstructionNormal
              currentInstruction -> currentInstruction
      EnterKey -> case pcsSelectedOption s of
        Just o -> case pcsConfirmation s of
          RequireConfirmation -> confirmSelection prompt o (goAgain s)
          DontConfirm -> do
            renderPrompt prompt
            withAttributes [bold, foreground yellow] . putTextLn $ chooseableItemText o
            pure o
        Nothing -> goAgain $ s {pcsInstruction = ChoiceInstructionNoOptionSelected}
      CharKey c ->
        if modifiers == mempty
          then
            if null (pcsFilteredOptions s)
              then goAgain $ s {pcsInstruction = ChoiceInstructionNoOptionSelected}
              else goAgain $ s & _pcsFilter %~ flip T.snoc c & applyFilter
          else goAgain s
      _ -> goAgain s
    _ -> goAgain s
  where
    goAgain :: PromptChoiceState requirement a -> m (PerRequirement requirement a)
    goAgain = promptChoiceInternal Proxy prompt

handlePromptTextEvent ::
  (MonadColorPrinter m, MonadFormattingPrinter m, MonadInput m, MonadScreen m) =>
  PromptTextState requirement ->
  Either Interrupt Event ->
  m (PerRequirement requirement Text)
handlePromptTextEvent s e = do
  deleteLines 1
  case e of
    Left Interrupt -> liftIO exitFailure
    Right (KeyEvent key modifiers) -> case key of
      BackspaceKey -> promptTextInternal $ s & _ptsCurrentInput %~ T.dropEnd 1
      EnterKey -> confirmTextInput s $ promptTextInternal s
      CharKey c ->
        if modifiers == mempty
          then promptTextInternal $ s & _ptsCurrentInput %~ (`T.snoc` c)
          else case c of
            'U'
              | modifiers == ctrlKey ->
                  promptTextInternal $ s {ptsCurrentInput = ""}
            'W'
              | modifiers == ctrlKey ->
                  promptTextInternal $ s & _ptsCurrentInput %~ (T.dropWhile C.isSpace . T.dropWhileEnd (not . C.isSpace))
            _ -> promptTextInternal s
      _ -> promptTextInternal s
    _ -> promptTextInternal s

-- | Applies the filter in `_pcsFilter` to set `_pcsOptions` and update `_pcsSelectedOption`
-- if the previously selected option is no longer included in the filter.
applyFilter ::
  (result ~ PerRequirement requirement a, ChooseableItem result, Eq result) =>
  PromptChoiceState requirement a ->
  PromptChoiceState requirement a
applyFilter s =
  let newFilteredOptions =
        filter
          ((T.toLower (pcsFilter s) `T.isInfixOf`) . T.toLower . chooseableItemText)
          (NE.toList $ pcsOptions s)
   in s
        & _pcsFilteredOptions .~ newFilteredOptions
        & _pcsSelectedOption %~ \case
          Just o | o `elem` newFilteredOptions -> Just o
          _ -> headViaNonEmpty newFilteredOptions

confirmSelection ::
  (ChooseableItem a, MonadColorPrinter m, MonadFormattingPrinter m, MonadInput m, MonadScreen m) =>
  Text ->
  a ->
  m a ->
  m a
confirmSelection prompt selection onCancel = do
  renderPromptLine prompt
  withAttributes [bold, foreground cyan] $ putText "You have chosen: "
  withAttributes [bold, foreground yellow] $ putTextLn (chooseableItemText selection)
  withAttributes [italic] $ putTextLn "Press enter again to confirm, or escape to go back."
  e <- awaitEvent
  moveCursorUp 3 *> deleteLines 4
  case e of
    Left Interrupt -> liftIO exitFailure
    Right (KeyEvent EnterKey _) -> do
      renderPrompt prompt
      withAttributes [bold, foreground yellow] $ putTextLn (chooseableItemText selection)
      pure selection
    Right (KeyEvent EscapeKey _) -> onCancel
    _ -> confirmSelection prompt selection onCancel

confirmTextInput ::
  forall m requirement.
  (MonadColorPrinter m, MonadFormattingPrinter m, MonadInput m, MonadScreen m) =>
  PromptTextState requirement ->
  m (PerRequirement requirement Text) ->
  m (PerRequirement requirement Text)
confirmTextInput s onCancel =
  if T.null selection && responseIsRequired
    then onCancel
    else case ptsConfirmation s of
      RequireConfirmation -> do
        writePromptWithResponse
        withAttributes [italic] $ putTextLn "Press enter again to confirm, or escape to go back."
        e <- awaitEvent
        moveCursorUp 2 *> deleteLines 3
        case e of
          Left Interrupt -> liftIO exitFailure
          Right (KeyEvent EnterKey _) -> do
            writePromptWithResponse
            returnCurrentInput
          Right (KeyEvent EscapeKey _) -> onCancel
          _ -> confirmTextInput s onCancel
      DontConfirm -> writePromptWithResponse *> returnCurrentInput
  where
    (prompt, selection) = (ptsPrompt s, T.strip $ ptsCurrentInput s)
    responseIsRequired = case ptsRequirement s of Required -> True; Optional -> False
    writePromptWithResponse :: m ()
    writePromptWithResponse = do
      renderPrompt prompt
      if T.null selection
        then withAttributes [foreground cyan] $ putTextLn "(You did not enter a response)"
        else withAttributes [bold, foreground yellow] $ putTextLn selection
    returnCurrentInput :: m (PerRequirement requirement Text)
    returnCurrentInput = pure $ case (ptsRequirement s, T.null selection) of
      (Required, _) -> selection
      (Optional, True) -> Nothing
      (Optional, False) -> Just selection

-- | Applies the given attributes to the terminal, runs the action,
-- then resets the terminal's attributes.
withAttributes :: (MonadMarkupPrinter m, Foldable t) => t (Attribute m) -> m a -> m a
withAttributes as action = do
  resetAttributes
  forM_ as setAttribute
  result <- action
  resetAttributes
  pure result

-- | Returns the first element, or `Nothing` if the list is empty.
headViaNonEmpty :: [a] -> Maybe a
headViaNonEmpty =
  \case
    Just (x :| _) -> Just x
    Nothing -> Nothing
    . NE.nonEmpty
