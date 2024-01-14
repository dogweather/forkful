---
title:                "Haskell: Création d'un fichier temporaire"
simple_title:         "Création d'un fichier temporaire"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/haskell/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Pourquoi

Créer un fichier temporaire peut être incroyablement utile lorsque vous travaillez sur des projets en Haskell. Cela peut vous permettre de stocker temporairement des données ou de créer des fichiers utilisés dans vos programmes. Dans cet article, nous allons vous montrer comment créer un fichier temporaire en utilisant Haskell.

## Comment faire

Pour créer un fichier temporaire en Haskell, nous allons utiliser la fonction `withSystemTempFile` disponible dans le module `System.IO`. Voici un exemple de code qui utilise cette fonction :

```Haskell
import System.IO

main = do
    (filePath, handle) <- withSystemTempFile "example.txt" $ \tempfile ->
        putStrLn tempfile
        return (tempfile, handle)
```

Dans cet exemple, nous importons le module `System.IO` qui contient les fonctions liées aux entrées/sorties. Ensuite, nous utilisons la fonction `withSystemTempFile` en lui passant le nom que nous voulons donner à notre fichier temporaire. La fonction retourne un tuple contenant le chemin vers le fichier et un handle que nous pouvons utiliser pour écrire dans ce fichier.

Dans notre exemple, nous utilisons la fonction `putStrLn` pour afficher le chemin vers le fichier temporaire créé. Ensuite, nous retournons le tuple contenant les informations sur notre fichier temporaire.

## Plongeons plus profondément

La fonction `withSystemTempFile` gère automatiquement la création et la suppression du fichier temporaire une fois que vous avez fini de l'utiliser. Cela garantit que vous n'aurez pas de fichiers temporaires inutiles qui traînent sur votre système.

De plus, la fonction prend en charge les différentes plates-formes sur lesquelles Haskell peut être utilisé. Elle veille à ce que les chemins de fichiers soient correctement gérés, quel que soit le système d'exploitation sur lequel vous travaillez.

## Voir aussi

- [Documentation officielle de la fonction `withSystemTempFile` (en anglais)](https://hackage.haskell.org/package/base-4.12.0.0/docs/System-IO.html#v:withSystemTempFile)
- [Tutoriel sur la gestion des fichiers en Haskell (en anglais)](https://www.schoolofhaskell.com/school/basic-haskell/10-things-you-should-know-about-haskell#files)
- [Un autre exemple d'utilisation de la fonction `withSystemTempFile` (en anglais)](https://stackoverflow.com/questions/6485362/defining-temporary-file-name-in-haskell)