---
title:                "La lecture d'un fichier texte"
html_title:           "Haskell: La lecture d'un fichier texte"
simple_title:         "La lecture d'un fichier texte"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/haskell/reading-a-text-file.md"
---

{{< edit_this_page >}}

Qu'est-ce que la Lecture d'un Fichier Texte en Haskell et Pourquoi les Programmeurs le Font

## Qu'est-ce que c'est et pourquoi les programmeurs le font?

Lire un fichier texte en Haskell ne signifie rien d'autre que lire le contenu d'un fichier texte et le stocker dans une variable pour être utilisé dans un programme. Les programmeurs le font souvent car il leur permet de travailler avec des données stockées dans des fichiers externes, plutôt que de les saisir manuellement dans leur code.

## Comment faire:

Voici un exemple de code pour lire un fichier texte en Haskell:

```Haskell
main = do
    file <- readFile "monfichier.txt" --on utilise la fonction readFile pour lire le fichier
    putStr file --on affiche le contenu du fichier
```

Supposons que notre fichier texte "monfichier.txt" contient le texte suivant:

```
Je suis un fichier texte.
Voici ma deuxième ligne.
```

Lorsque nous exécutons notre programme, nous obtiendrons la sortie suivante:

```
Je suis un fichier texte.
Voici ma deuxième ligne.
```

## Plongée en profondeur:

Lire un fichier texte en Haskell est une tâche assez simple, mais il y a quelques choses à garder à l'esprit. Tout d'abord, assurez-vous que votre fichier texte se trouve dans le même répertoire que votre programme Haskell. Sinon, vous devrez spécifier le chemin complet lors de l'utilisation de la fonction readFile.

Deuxièmement, il est important de noter que la fonction readFile renvoie une chaîne de caractères contenant le contenu du fichier. Si vous avez besoin de travailler avec les données dans un format différent, vous devrez les convertir en utilisant des fonctions appropriées. Par exemple, si vous avez besoin de convertir les données en une liste de nombres, vous pouvez utiliser la fonction "read" comme suit:

```Haskell
main = do
    file <- readFile "numbers.txt"
    let numbers = map read $ lines file :: [Int] --convertit les données en une liste de nombres entiers
    print numbers --affiche la liste de nombres
```

Enfin, sachez qu'il existe d'autres façons de lire des fichiers en Haskell, telles que l'utilisation de la fonction "getContents" pour lire le contenu d'un fichier sans avoir à spécifier son nom. Vous pouvez également utiliser des outils bibliothèques tels que "Data.Text" pour gérer des fichiers texte plus volumineux.

## Voir aussi:

Pour en savoir plus sur la lecture de fichiers en Haskell, consultez la documentation officielle sur [la lecture et l'écriture de fichiers en Haskell](https://www.haskell.org/tutorial/io.html). Vous pouvez également jeter un œil à [cette présentation](https://www.slideshare.net/MarcosVinicius196/reading-and-writing-files-in-haskell) qui explique en détail les différentes façons de lire et écrire des fichiers en Haskell.