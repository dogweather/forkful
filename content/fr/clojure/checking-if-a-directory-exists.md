---
title:                "Vérifier si un répertoire existe"
html_title:           "Clojure: Vérifier si un répertoire existe"
simple_title:         "Vérifier si un répertoire existe"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/clojure/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Qu'est-ce que c'est et pourquoi?

Vérifier si un répertoire existe consiste simplement à s'assurer que le chemin vers un dossier spécifié piste bien vers un emplacement réel sur le système de fichiers. C’est essentiel pour éviter les erreurs lors de l'écriture, de la lecture ou du déplacement de fichiers.

## Comment faire :

Utiliser `java.io.File` avec `exists` et `directory?` de Clojure. Le premier vérifie si le chemin existe, le deuxième confirme si c'est un répertoire.

```Clojure
(defn directory-exists? [dir-path]
  (let [dir (java.io.File. dir-path)]
    (and (.exists dir) (.isDirectory dir))))
```

Exemple d'utilisation :

```Clojure
(directory-exists? "/mon/chemin/vide")
; => false

(directory-exists? "/chemin/existant")
; => true
```

## Deep Dive :

`java.io.File` a été présent dans Java depuis JDK 1.0, donc c'est une solution éprouvée. Cependant, notez que depuis Java 7, nous avons une alternative, `java.nio.file.Files` avec la méthode `exists` et `isDirectory`, qui est plus moderne et flexible.

Détails de mise en œuvre : `java.io.File.exists()` vérifie simplement l'accessibilité en lecture des fichiers. Ce n'est donc pas une méthode fiable pour vérifier les permissions complètes sur un répertoire. Si vous avez besoin de plus de contrôle, regardez du côté de `java.nio.file.Files`.

## À Voir :

1. [Java File Docs](https://docs.oracle.com/javase/8/docs/api/java/io/File.html)
2. [Clojure Java Interop](https://clojure.org/reference/java_interop)
3. [Java Nio Files](https://docs.oracle.com/javase/8/docs/api/java/nio/file/Files.html)