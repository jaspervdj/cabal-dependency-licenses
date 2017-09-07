{-# LANGUAGE CPP #-}
--------------------------------------------------------------------------------
module Main
    ( main
    ) where


--------------------------------------------------------------------------------
import           Control.Monad                      (forM_, when)
import           Data.List                          (foldl', sortBy)
import           Data.Maybe                         (catMaybes)
import           Data.Ord                           (comparing)
import           Data.Set                           (Set)
import qualified Data.Set                           as Set
import qualified Data.Map                           as Map
import           Distribution.InstalledPackageInfo  (InstalledPackageInfo)
import qualified Distribution.InstalledPackageInfo  as InstalledPackageInfo
import qualified Distribution.License               as Cabal
import qualified Distribution.Package               as Cabal
import qualified Distribution.Simple.Configure      as Cabal
import qualified Distribution.Simple.LocalBuildInfo as Cabal
import qualified Distribution.Simple.PackageIndex   as Cabal
import qualified Distribution.Text                  as Cabal
import           System.Directory                   (doesDirectoryExist, doesFileExist)
import           System.Exit                        (exitFailure)
import           System.FilePath                    ((</>), takeDirectory)

import qualified System.FilePath.Find as F

--------------------------------------------------------------------------------
existsDistDir :: IO Bool
existsDistDir = do
    e <- doesDirectoryExist "dist"
    f <- doesFileExist $ "dist" </> "setup-config"
    return $ e && f

existsDistNewstyleDir :: IO Bool
existsDistNewstyleDir = doesDirectoryExist "dist-newstyle"

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
    -> Set Cabal.InstalledPackageId
    -> Set Cabal.InstalledPackageId
findTransitiveDependencies pkgIdx set0 = go Set.empty (Set.toList set0)
  where
    go set []  = set
    go set (q : queue)
        | q `Set.member` set = go set queue
        | otherwise          =
            case Cabal.lookupInstalledPackageId pkgIdx q of
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
    :: Cabal.LocalBuildInfo -> Set Cabal.InstalledPackageId
getDependencyInstalledPackageIds lbi =
    findTransitiveDependencies (Cabal.installedPkgs lbi) $
        Set.fromList
            [ installedPackageId
            | componentLbi    <- concat (Map.elems (Cabal.componentNameMap lbi))
            , (installedPackageId, _) <- Cabal.componentPackageDeps componentLbi
            ]


--------------------------------------------------------------------------------
getDependencyInstalledPackageInfos
    :: Cabal.LocalBuildInfo -> [InstalledPackageInfo]
getDependencyInstalledPackageInfos lbi = catMaybes $
    map (Cabal.lookupInstalledPackageId pkgIdx) $
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
                copyrightStr = InstalledPackageInfo.copyright ipi
                crNotice = case null copyrightStr of
                    True -> ", no copyright notice available"
                    False -> ", Copyright " ++ copyrightStr
            putStrLn $ "- " ++ getName ipi ++ " (" ++ synopsis ++ crNotice ++ ")"
        putStrLn ""
  where
    getName =
        Cabal.display . Cabal.pkgName . InstalledPackageInfo.sourcePackageId

    getSynopsis = InstalledPackageInfo.synopsis

handleDist :: IO ()
handleDist = do
    -- Get info and print dependency license list
    lbi <- Cabal.getPersistBuildConfig "dist"
    printDependencyLicenseList $
        groupByLicense $
        getDependencyInstalledPackageInfos lbi

handleDistNewstyle :: IO ()
handleDistNewstyle = do
    configs <- F.find (return True) (F.fileName F.==? "setup-config") "dist-newstyle"
    forM_ configs $ \configPath -> do
        lbi <- Cabal.getPersistBuildConfig $ takeDirectory configPath
        printDependencyLicenseList $
            groupByLicense $
            getDependencyInstalledPackageInfos lbi

--------------------------------------------------------------------------------
main :: IO ()
main = do
    existsDist <- existsDistDir
    existsDistNewstyle <- existsDistNewstyleDir

    when (not $ existsDist || existsDistNewstyle) $ do
        putStrLn "Error: dist or dist-newstyle not found"
        exitFailure

    when existsDist handleDist
    when existsDistNewstyle handleDistNewstyle
