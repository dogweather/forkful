---
title:                "Vérification de l'existence d'un répertoire"
date:                  2024-01-19
html_title:           "Bash: Vérification de l'existence d'un répertoire"
simple_title:         "Vérification de l'existence d'un répertoire"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/clojure/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## What & Why?
Vérifier l'existence d'un répertoire permet de s'assurer que le chemin d'accès utilisé est valide et d'éviter les erreurs de fichier. Les programmeurs le font pour des raisons de sécurité et de robustesse dans le flux de travail de leurs applications.

## How to:
Utiliser `clojure.java.io/file` et `clojure.java.io/exists?` pour vérifier si un répertoire existe :

```Clojure
(require '[clojure.java.io :as io])

(defn directory-exists? [dir-path]
  (let [dir (io/file dir-path)]
    (.exists dir)))

(println (directory-exists? "/path/to/your/directory")) ; Renvoie true si le répertoire existe, false sinon.
```

Sample output: 
```
true
```

## Deep Dive
Historiquement, la gestion des fichiers et répertoires en Clojure se fait grâce aux abstractions sur les entrées/sorties de Java. `clojure.java.io` est un wrapper autour des classes Java standard pour les fichiers.

Alternatives: Pour ceux qui préfèrent les solutions natives, `java.nio.file.Files` et `java.nio.file.Paths` offrent des fonctions modernes et plus riches en Java, accessibles depuis Clojure.

Détails d'implémentation: `clojure.java.io/file` crée une instance `java.io.File`, tandis que `exists?` appelle la méthode `.exists` de l'instance pour vérifier l'existence physique du fichier ou répertoire.

## See Also
- Clojure Documentation: https://clojure.org/
- API clojure.java.io: https://clojuredocs.org/clojure.java.io
- Java NIO Files: https://docs.oracle.com/javase/8/docs/api/java/nio/file/Files.html
- Guide sur les fichiers et répertoires en Clojure: https://www.braveclojure.com/clojure-for-the-brave-and-true/
