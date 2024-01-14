---
title:    "Haskell: Créer un fichier temporaire"
keywords: ["Haskell"]
---

{{< edit_this_page >}}

## Pourquoi

La création de fichiers temporaires peut être utile pour stocker temporairement des données ou des fichiers lors de l'exécution d'un programme Haskell. Cela peut également être nécessaire pour effectuer des tests et des opérations de fichiers de manière sécurisée.

## Comment faire

Il existe plusieurs façons de créer des fichiers temporaires en Haskell, mais l'une des plus courantes est d'utiliser la bibliothèque standard "System.IO". Voici un exemple de code pour créer un fichier temporaire et y écrire du contenu :

```Haskell
import System.IO

main = do
    -- Créer un fichier temporaire
    (tempFileName, tempFileHandle) <- openTempFile "." "temp"

    -- Écrire du contenu dans le fichier
    hPutStrLn tempFileHandle "Ceci est du contenu temporaire."

    -- Fermer le fichier
    hClose tempFileHandle

    -- Afficher le nom du fichier temporaire créé
    putStrLn $ "Fichier temporaire créé : " ++ tempFileName
```

L'exemple ci-dessus utilise la fonction "openTempFile" pour créer un fichier temporaire dans le répertoire courant avec le préfixe "temp". Ensuite, la fonction "hPutStrLn" est utilisée pour écrire du contenu dans le fichier, suivi de la fermeture du fichier avec la fonction "hClose". Enfin, le nom du fichier temporaire est affiché à l'aide de la fonction "putStrLn".

## Plongée en profondeur

Lors de la création d'un fichier temporaire, il est important de s'assurer qu'il est supprimé après utilisation pour éviter d'encombrer l'espace de stockage. Pour ce faire, vous pouvez utiliser la fonction "removeFile" de la bibliothèque "System.Directory". Voici un exemple de code pour créer un fichier temporaire, y écrire du contenu et le supprimer ensuite :

```Haskell
import System.IO
import System.Directory

main = do
    -- Créer un fichier temporaire
    (tempFileName, tempFileHandle) <- openTempFile "." "temp"

    -- Écrire du contenu dans le fichier
    hPutStrLn tempFileHandle "Ceci est du contenu temporaire."

    -- Fermer le fichier
    hClose tempFileHandle

    -- Afficher le nom du fichier temporaire créé
    putStrLn $ "Fichier temporaire créé : " ++ tempFileName

    -- Supprimer le fichier temporaire
    removeFile tempFileName
```

En utilisant la fonction "removeFile", le fichier temporaire sera supprimé après utilisation, laissant ainsi votre espace de stockage propre et organisé.

## Voir aussi

- [Documentation de la bibliothèque System.IO](https://hackage.haskell.org/package/base/docs/System-IO.html)
- [Documentation de la bibliothèque System.Directory](https://hackage.haskell.org/package/directory/docs/System-Directory.html)
- [Guide Haskell pour débutants](https://wiki.haskell.org/Beginners_exercises)