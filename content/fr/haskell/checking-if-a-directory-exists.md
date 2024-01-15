---
title:                "Vérifier l'existence d'un répertoire"
html_title:           "Haskell: Vérifier l'existence d'un répertoire"
simple_title:         "Vérifier l'existence d'un répertoire"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/haskell/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Pourquoi
Si vous travaillez avec des fichiers et des dossiers dans votre code Haskell, il peut être utile de vérifier si un dossier existe avant de continuer avec votre code. Cela peut vous éviter des erreurs et des bugs potentiels.

## Comment faire
Pour vérifier si un dossier existe en Haskell, nous allons utiliser la fonction `doesDirectoryExist` du module `System.Directory`. Voici un exemple de code avec une sortie d'exemple:

```Haskell
import System.Directory

main = do
  let directory = "monDossier/"
  exists <- doesDirectoryExist directory
  if exists
    then putStrLn (directory ++ " existe!")
    else putStrLn (directory ++" n'existe pas!")

-- Output: "monDossier/ n'existe pas!"
```

## Plongez plus profondément
En utilisant la fonction `doesDirectoryExist`, il est également possible de vérifier si un fichier ou un lien symbolique existe. La fonction renverra `True` si un fichier ou un dossier avec le même nom existe, même s'il ne s'agit pas d'un dossier. De plus, il est important de noter que cette fonction effectue une vérification synchrone, ce qui signifie que votre code sera en pause jusqu'à ce que la vérification soit terminée.

## Voir aussi
- [Documentation sur la fonction `doesDirectoryExist`](https://hackage.haskell.org/package/directory-1.3.6.1/docs/System-Directory.html#v:doesDirectoryExist)
- [Exemple de gestion d'erreurs de fichiers en Haskell](https://stackoverflow.com/questions/33181843/handling-file-errors-in-haskell)