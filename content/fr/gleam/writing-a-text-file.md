---
title:                "Gleam: Écrire un fichier texte"
programming_language: "Gleam"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/gleam/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Pourquoi

La création d'un fichier texte est un élément essentiel de la programmation en Gleam. Cela permet de stocker et de manipuler des données, ainsi que de les utiliser dans le code pour réaliser des tâches spécifiques.

## Comment faire

Pour créer un fichier texte en Gleam, il faut utiliser le module `file`, qui fournit des fonctions pour créer, lire et écrire dans un fichier. Voici un exemple de code pour écrire dans un fichier texte :

```Gleam
import file.{write}

let message = "Bonjour tout le monde!"
file.write("message.txt", message)
```

Cette fonction va créer un fichier texte appelé "message.txt" et y écrire le contenu de la variable `message`. Vous pouvez également écrire du contenu directement sans utiliser de variable :

```Gleam
import file.{write}

file.write("intro.txt", "Bienvenue sur mon blog de programmation!")
```

Pour lire le contenu d'un fichier texte, vous pouvez utiliser la fonction `read` :

```Gleam
import file.{read}

let contenu = file.read("message.txt")
```

La variable `contenu` va contenir le contenu du fichier "message.txt". Vous pouvez ensuite l'utiliser dans d'autres parties de votre code.

## Plongée en profondeur

En plus des fonctions de base pour écrire et lire des fichiers, le module `file` offre également des fonctionnalités avancées pour la manipulation de fichiers. Par exemple, vous pouvez déplacer, copier ou supprimer des fichiers en utilisant les fonctions `move`, `copy` et `delete`.

De plus, vous pouvez également travailler avec des fichiers compressés en utilisant les fonctions `zip` et `unzip` pour créer et extraire des fichiers zip.

## Voir aussi

- Documentation officielle de Gleam sur les fichiers : https://gleam.run/documentation/stdlib/file
- Exemples de code pour la manipulation de fichiers en Gleam : https://github.com/gleam-lang/gleam/blob/master/lib/file/tests/file_test.gleam