-- This file was generated by nix-bootstrap.
-- It should be checked into version control.
-- It is used to aid migration between nix-bootstrap versions and preserve idempotence.
let NodePackageManager = < NPM | PNPm | Yarn >

let ElmMode = < Bare | Node : NodePackageManager >

let ElmOptions = { elmMode : ElmMode, provideElmReview : Bool }

let HaskellProjectType = < ReplOnly | Basic : Bool >

let HaskellOptions =
      { ghcVersion : { major : Natural, minor : Natural, patch : Natural }
      , haskellProjectType : HaskellProjectType
      }

let JavaOptions =
      { installMinishift : Bool
      , installLombok : Bool
      , setUpJavaBuild : < SetUpJavaBuild : Text | NoJavaBuild >
      , jdk : < OpenJDK | GraalVM >
      }

let ProjectType =
      < Minimal
      | Elm : ElmOptions
      | Haskell : HaskellOptions
      | Node : NodePackageManager
      | Go : Bool
      | Java : JavaOptions
      | Python
      | Rust
      >

in  { projectName = "prompt-hs"
    , projectType =
        ProjectType.Haskell
          { ghcVersion = { major = 9, minor = 8, patch = 4 }
          , haskellProjectType = HaskellProjectType.Basic True
          }
    , setUpPreCommitHooks = True
    , setUpContinuousIntegration = False
    , setUpVSCodeDevContainer = False
    }
