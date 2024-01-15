---
title:                "Vérification de l'existence d'un répertoire"
html_title:           "Clojure: Vérification de l'existence d'un répertoire"
simple_title:         "Vérification de l'existence d'un répertoire"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/clojure/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Pourquoi 

Checking if a directory exists is an important aspect of file operations in any programming language. It helps ensure that your code can handle different scenarios and avoids any potential errors that may occur if a directory does not exist. In Clojure, there are different ways to check for the existence of a directory, making it a useful skill to have in your programming toolkit.

## Comment faire 

Pour vérifier si un dossier existe en Clojure, nous pouvons utiliser la fonction `exists?` du module `clojure.java.io`. Cette fonction prend en paramètre le chemin absolu du dossier et renvoie `true` si le dossier existe ou `false` s'il n'existe pas. Voici un exemple de code avec son résultat :

```Clojure
(require '[clojure.java.io :as io])

(io/exists? "/chemin/absolu/vers/dossier") ; renvoie true
```

Nous pouvons également utiliser la fonction `file-seq` du module `clojure.java.io` pour parcourir le contenu d'un dossier et vérifier chaque élément s'il s'agit d'un fichier ou d'un dossier. Voici un exemple de code avec son résultat :

```Clojure
(require '[clojure.java.io :as io])

(def files (file-seq "/chemin/absolu/vers/dossier"))

(some #(and (io/directory? %) (str %)) files) ; renvoie le chemin absolu du dossier s'il existe
```

## Zoom sur 

En utilisant la fonction `file-seq`, nous pouvons également faire une recherche plus approfondie en utilisant des prédicats de filtrage pour vérifier des conditions spécifiques sur les fichiers ou dossiers dans un répertoire donné. Par exemple, nous pouvons utiliser `filter` pour vérifier si un fichier avec une certaine extension existe dans un dossier. Voici un exemple :

```Clojure
(require '[clojure.java.io :as io])

(def files (file-seq "/chemin/absolu/vers/dossier"))

(filter #(and (io/file? %) (.endsWith (.getName %) "txt")) files) ; renvoie tous les fichiers avec l'extension .txt dans le dossier
```

## Voir aussi 

- [Documentation de Clojure : clojure.java.io](https://clojure.github.io/clojure/clojure.java.io-api.html)
- [Documentation de Java : java.io.File](https://docs.oracle.com/javase/8/docs/api/java/io/File.html)
- [Article de blog : Quelques astuces pour travailler avec des fichiers en Clojure](https://bbtmp.github.io/files-in-clojure/)