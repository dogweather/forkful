---
title:                "Lecture d'un fichier texte"
html_title:           "Clojure: Lecture d'un fichier texte"
simple_title:         "Lecture d'un fichier texte"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/clojure/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Quoi & Pourquoi?
Lire un fichier texte est une tâche courante pour les programmeurs. Cela consiste à accéder et à extraire des données à partir d'un fichier texte, ce qui peut être utile pour effectuer des traitements ultérieurs.

## Comment faire:
Il existe plusieurs façons de lire un fichier texte en Clojure. L'une des méthodes les plus simples consiste à utiliser la fonction `slurp`, qui permet de lire directement le contenu d'un fichier en une seule ligne de code. Par exemple:

```Clojure
(def texte (slurp "monFichier.txt"))
```

Cela stockera le contenu du fichier texte "monFichier.txt" dans une variable appelée "texte". Vous pouvez ensuite manipuler cette variable comme n'importe quelle chaîne de caractères en utilisant des fonctions de chaînes de caractères telles que `split` ou `substring`.

Vous pouvez également utiliser les fonctions de la bibliothèque standard de Clojure, telles que `with-open`, pour lire un fichier texte ligne par ligne. Par exemple:

```Clojure
(with-open [f (clojure.java.io/reader "monFichier.txt")]
  (doseq [ligne (line-seq f)]
    (prn ligne)))
```

Cela imprimera chaque ligne du fichier texte "monFichier.txt" sur une nouvelle ligne dans la console.

## Plongée en profondeur:
Lire des fichiers texte a été une fonctionnalité de base des langages de programmation depuis longtemps. Cela est dû à la nature omniprésente des fichiers texte dans les systèmes d'exploitation et leur utilité pour stocker des données.

Il existe également d'autres bibliothèques Clojure telles que `pandect`, `flatland/useful` et `data.csv` qui permettent de lire des fichiers texte d'une manière plus spécifique à un cas d'utilisation particulier. Il est important de choisir le bon outil pour la tâche à accomplir.

Lorsqu'on lit un fichier texte en Clojure, il est important de comprendre le type de données renvoyé par les différentes fonctions. Par exemple, `slurp` renvoie une chaîne de caractères, tandis que `line-seq` renvoie une séquence de lignes.

## Voir aussi:
- [Documentation officielle de Clojure sur la lecture de fichiers](https://clojuredocs.org/clojure.core/read-string)
- [Tutoriel vidéo "Reading Files in Clojure" par Technomancy](https://www.youtube.com/watch?v=p4wff_iRZeQ)
- [Documentation de la bibliothèque Pandect](https://cljdoc.org/d/pandect/pandect/0.5.0/doc/pandect.read)
- [Documentation de la bibliothèque flatland/useful](https://cljdoc.org/d/flatland/useful/0.14.2/doc/read-file)
- [Documentation de la bibliothèque data.csv](https://cljdoc.org/d/data.csv/data.csv/1.0.0/doc/read-csv-file)