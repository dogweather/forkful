---
title:                "Clojure: Lecture d'un fichier texte."
simple_title:         "Lecture d'un fichier texte."
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/clojure/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Pourquoi

La lecture de fichiers texte est une étape cruciale dans de nombreux programmes, permettant de manipuler et de traiter des données externes. Comprendre comment lire des fichiers texte en Clojure peut vous aider à créer des applications plus complètes et plus efficaces.

## Comment faire

La première étape pour lire un fichier texte en Clojure est d'ouvrir le fichier à l'aide de la fonction `with-open`. Cette fonction prend deux arguments : le chemin d'accès au fichier et un nom de variable que vous utiliserez pour référencer le fichier ouvert.

```Clojure
(with-open [fichier (io/reader "chemin/vers/le/fichier.txt")]
  ;; Vous pouvez maintenant exécuter des opérations sur le fichier ici
  )
```

Ensuite, vous pouvez utiliser la fonction `line-seq` pour lire le contenu du fichier ligne par ligne dans une séquence. Vous pouvez également utiliser la fonction `read-line` pour lire une seule ligne à la fois.

```Clojure
(with-open [fichier (io/reader "chemin/vers/le/fichier.txt")]
  (doseq [ligne (line-seq fichier)]
    ;; Faire quelque chose avec chaque ligne ici
    ))
```

Il est également possible de spécifier l'encodage du fichier en ajoutant un troisième argument à `io/reader`.

```Clojure
(io/reader "chemin/vers/le/fichier.txt" "utf-8")
```

## Plongée en profondeur

Lorsque vous lisez un fichier texte en Clojure, il est important de prendre en compte l'encodage du fichier afin de vous assurer que le texte est correctement interprété. Dans certains cas, il peut être nécessaire de convertir l'encodage du fichier en utilisant la fonction `clojure.string/encoding`.

Il est également possible de lire des fichiers compressés en utilisant la fonction `gzip-reader` ou `bzip2-reader` à la place de `io/reader`.

## Voir aussi

- [Documentation de Clojure sur la lecture de fichiers](https://clojure.org/reference/io)
- [Article sur la manipulation des fichiers en Clojure](https://oli.me.uk/2011/01/06/reading-a-file-in-clojure/)
- [Exemples de code pour la lecture de fichiers en Clojure](https://gist.github.com/karianna/647202)