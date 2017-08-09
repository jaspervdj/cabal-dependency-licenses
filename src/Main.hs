{-# LANGUAGE CPP #-}
--------------------------------------------------------------------------------
module Main
    ( main
    ) where


--------------------------------------------------------------------------------
import           Control.Monad                      (forM_, unless)
import           Data.Foldable                      (toList)
import           Data.List                          (foldl', sortBy)
import           Data.Maybe                         (catMaybes)
import           Data.Ord                           (comparing)
import           Data.Set                           (Set)
import qualified Data.Set                           as Set
import           Distribution.InstalledPackageInfo  (InstalledPackageInfo)
import qualified Distribution.InstalledPackageInfo  as InstalledPackageInfo
import qualified Distribution.License               as Cabal
import qualified Distribution.Package               as Cabal
import qualified Distribution.Simple.Configure      as Cabal
import qualified Distribution.Simple.LocalBuildInfo as Cabal
import qualified Distribution.Simple.PackageIndex   as Cabal
import qualified Distribution.Text                  as Cabal
import           System.Directory                   (getDirectoryContents)
import           System.Exit                        (exitFailure)
import           System.FilePath                    (takeExtension)
import           System.IO                          (hPutStrLn, stderr)


--------------------------------------------------------------------------------
putErrLn :: String -> IO ()
putErrLn = hPutStrLn stderr


--------------------------------------------------------------------------------
existsCabalFile :: IO Bool
existsCabalFile = do
    contents <- getDirectoryContents "."
    return $ any ((== ".cabal") . takeExtension) contents

existsDistNewstyleDir :: IO Bool
existsDistNewstyleDir = do
    contents <- getDirectoryContents "."
    return $ any (== "dist-newstyle") contents

--------------------------------------------------------------------------------
#if MIN_VERSION_Cabal(1,24,0)
type PackageIndex a = Cabal.PackageIndex InstalledPackageInfo.InstalledPackageInfo
#elif MIN_VERSION_Cabal(1,22,0)
type PackageIndex a = Cabal.PackageIndex (InstalledPackageInfo.InstalledPackageInfo_ a)
#else
type PackageIndex a = Cabal.PackageIndex
#endif

findTransitiveDependencies
    :: PackageIndex a
    -> Set Cabal.UnitId
    -> Set Cabal.UnitId
findTransitiveDependencies pkgIdx set0 = go Set.empty (Set.toList set0)
  where
    go set []  = set
    go set (q : queue)
        | q `Set.member` set = go set queue
        | otherwise          =
            case Cabal.lookupUnitId pkgIdx q of
                Nothing  ->
                    -- Not found can mean that the package still needs to be
                    -- installed (e.g. a component of the target cabal package).
                    -- We can ignore those.
                    go set queue
                Just ipi ->
                    go (Set.insert q set)
                        (InstalledPackageInfo.depends ipi ++ queue)


--------------------------------------------------------------------------------
getDependencyInstalledPackageIds
    :: Cabal.LocalBuildInfo -> Set Cabal.UnitId
getDependencyInstalledPackageIds lbi =
    findTransitiveDependencies (Cabal.installedPkgs lbi) $
        Set.fromList
            [ installedPackageId
            | (componentLbi)    <- toList (Cabal.componentGraph lbi)
            , (installedPackageId, _) <- Cabal.componentPackageDeps componentLbi
            ]


--------------------------------------------------------------------------------
getDependencyInstalledPackageInfos
    :: Cabal.LocalBuildInfo -> [InstalledPackageInfo]
getDependencyInstalledPackageInfos lbi = catMaybes $
    map (Cabal.lookupUnitId pkgIdx) $
    Set.toList (getDependencyInstalledPackageIds lbi)
  where
    pkgIdx = Cabal.installedPkgs lbi


--------------------------------------------------------------------------------
groupByLicense
    :: [InstalledPackageInfo]
    -> [(Cabal.License, [InstalledPackageInfo])]
groupByLicense = foldl'
    (\assoc ipi -> insert (InstalledPackageInfo.license ipi) ipi assoc) []
  where
    -- 'Cabal.License' doesn't have an 'Ord' instance so we need to use an
    -- association list instead of 'Map'. The number of licenses probably won't
    -- exceed 100 so I think we're alright.
    insert :: Eq k => k -> v -> [(k, [v])] -> [(k, [v])]
    insert k v []   = [(k, [v])]
    insert k v ((k', vs) : kvs)
        | k == k'   = (k, v : vs) : kvs
        | otherwise = (k', vs) : insert k v kvs


--------------------------------------------------------------------------------
printDependencyLicenseList
    :: [(Cabal.License, [InstalledPackageInfo])]
    -> IO ()
printDependencyLicenseList byLicense =
    forM_ byLicense $ \(license, ipis) -> do
        putStrLn $ "# " ++ Cabal.display license
        putStrLn ""

        let sorted = sortBy (comparing getName) ipis
        forM_ sorted $ \ipi -> do
            let synopsis = getSynopsis ipi
            putStrLn $ "- " ++ getName ipi ++ " (" ++ synopsis ++ ")"
        putStrLn ""
  where
    getName =
        Cabal.display . Cabal.pkgName . InstalledPackageInfo.sourcePackageId

    getSynopsis = InstalledPackageInfo.synopsis


--------------------------------------------------------------------------------
main :: IO ()
main = do
    -- Check that we're in a directory with a cabal file
    existsCabalFile' <- existsCabalFile
    unless existsCabalFile' $ do
        putErrLn "No cabal file found in the current directory"
        exitFailure

    -- Check if dist-newstyle should be preferred to dist
    existsDistNewstyleDir' <- existsDistNewstyleDir

    -- Get info and print dependency license list
    lbi <- Cabal.getPersistBuildConfig (if existsDistNewstyleDir' then "dist-newstyle" else "dist")
    printDependencyLicenseList $
        groupByLicense $
        getDependencyInstalledPackageInfos lbi
  where
    usage = do
      putErrLn "Usage: cabal-dependency-licenses [DIST]"
      putErrLn "  Print licensing information for the package described in *.cabal & configured in DIST (default: './dist')"
      exitFailure
