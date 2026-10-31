## Resubmission

This is a resubmission. In this version, the following corrections have been made:

- Replaced \dontrun{} with \donttest{} or unwrapped examples where appropriate
- Replaced cat()/print() console output with message() so output can be suppressed by the user
- (previous fix carried forward) Fixed Unicode character causing LaTeX PDF manual build errors

## R CMD check results

0 errors \| 0 warnings \| 0 notes

## Test environments

- local: Windows 11, R 4.6.1 — devtools::check() and devtools::check(args = "--as-cran"), both clean
- GitHub Actions (ubuntu-latest): R release, R devel, R oldrel-1
- GitHub Actions (windows-latest): R release
- GitHub Actions (macos-latest): R release

All GitHub Actions check runs passed with 0 errors, 0 warnings, 0 notes. See: <https://github.com/hkim-fda/MLSurvey/actions/runs/41161205469> (commit edacf96 )

## Downstream dependencies

There are no downstream dependencies.

## Additional notes

- This package depends on compiled-code packages (xgboost, randomForest); no platform-specific build issues were observed across the test matrix above.
- win-builder could not be reached from the maintainer's network (FTP port 21 blocked); Windows coverage instead comes from GitHub Actions windows-latest, R release.
