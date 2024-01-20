---
title:                "Écriture d'un fichier texte"
html_title:           "Haskell: Écriture d'un fichier texte"
simple_title:         "Écriture d'un fichier texte"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/haskell/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Quoi & pourquoi?
Écrire un fichier texte est l'action de créer et d'enregistrer un document contenant du texte, généralement utilisé par les programmeurs pour stocker des données ou du contenu à des fins de traitement ou de manipulation. Cela peut être fait dans n'importe quel langage de programmation, y compris Haskell.

## Comment faire:
Pour écrire un fichier texte en Haskell, utilisez la fonction `writeFile` en fournissant le chemin du fichier et le contenu que vous souhaitez y insérer. Par exemple:

```Haskell
writeFile "exemple.txt" "Ceci est un exemple de texte"
```

Cela créera un fichier texte nommé "exemple.txt" dans le répertoire où se trouve votre programme Haskell, et y écrira le contenu spécifié. Vous pouvez également créer un nouveau fichier en utilisant la fonction `openFile` et en spécifiant le mode d'ouverture de fichier `WriteMode`.

```Haskell
import System.IO
  
main = do
  file <- openFile "nouveau.txt" WriteMode
  hPutStrLn file "Ceci est un nouveau fichier"
  hClose file
```

Le code ci-dessus crée un nouveau fichier nommé "nouveau.txt" et y ajoute le texte spécifié. N'oubliez pas de fermer le fichier après utilisation avec la fonction `hClose` pour éviter les problèmes de mémoire.

## Plongée en profondeur:
Écrire des fichiers texte a été une étape cruciale dans l'histoire de la programmation, car cela a permis aux programmeurs de stocker des données pour une utilisation ultérieure. Il existe également d'autres alternatives pour stocker des données, telles que les bases de données et les fichiers binaires, mais écrire des fichiers texte reste une méthode simple et couramment utilisée. En ce qui concerne l'implémentation en Haskell, la fonction `writeFile` utilise le type `String` pour le contenu du fichier, et la fonction `openFile` utilise le type `Handle` pour représenter le fichier ouvert.

## Voir aussi:
- [Documentation officielle de l'utilisation des fichiers en Haskell](https://www.haskell.org/tutorial/io.html)