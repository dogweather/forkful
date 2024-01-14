---
title:                "Haskell: Création d'un fichier temporaire"
programming_language: "Haskell"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/haskell/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Pourquoi

La création de fichiers temporaires est une tâche commune en programmation pour stocker des données temporaires ou effectuer des opérations temporaires. Cela peut être utile pour éviter de remplir l'espace de stockage avec des données inutiles ou pour maintenir la propreté de vos données en supprimant les fichiers temporaires après leur utilisation.

## Comment faire

La création d'un fichier temporaire en Haskell est simple en utilisant la bibliothèque standard `System.IO.Temp`. Tout d'abord, il faut importer le module `System.IO.Temp` dans votre fichier Haskell.

```
import System.IO.Temp
```

Ensuite, utilisez la fonction `withSystemTempFile` pour créer un fichier temporaire.

```
withSystemTempFile "exemple.txt" $ \file handle -> do
    hPutStrLn handle "Ceci est un exemple de contenu."
    hFlush handle
    putStrLn $ "Fichier temporaire créé: " ++ file
```

Dans cet exemple, nous créons un fichier temporaire nommé "exemple.txt", écrivons une ligne de texte à l'intérieur, et imprimons le nom du fichier créé. Une fois le bloc d'actions terminé, le fichier temporaire sera automatiquement supprimé.

## Plongée en profondeur

La fonction `withSystemTempFile` prend en premier argument un préfixe de nom de fichier optionnel, suivi d'un bloc d'actions à effectuer sur le fichier temporaire créé. Ce bloc d'actions reçoit en paramètres le chemin d'accès au fichier et un gestionnaire de fichier ouvert pour le fichier.

Il est également possible d'utiliser la fonction `withTempFile` pour créer un fichier temporaire dans un répertoire spécifique.

``` 
withTempFile "chemin/vers/le/répertoire" "exemple.txt" $ \file handle -> do
    -- actions à effectuer sur le fichier
```

Vous pouvez également spécifier un suffixe pour le fichier temporaire en utilisant la fonction `openTempFile`.

```
(file, handle) <- openTempFile "chemin/vers/le/répertoire" "exemple" -- crée un fichier "exemple123" avec un numéro unique
```

Enfin, il est important de noter que la bibliothèque `System.IO.Temp` gère automatiquement la suppression des fichiers temporaires après leur utilisation. Cela peut être une tâche fastidieuse à gérer manuellement, mais grâce à cette bibliothèque, cela devient beaucoup plus simple et plus sûr.

## Voir aussi

- [Documentation de System.IO.Temp](https://hackage.haskell.org/package/temp-1.2.3.1/docs/System-IO-Temp.html)
- [Tutorial sur la création de fichiers temporaires en Haskell](https://wiki.haskell.org/Tutorials/Programming_Haskell/Files#Temporary_files)