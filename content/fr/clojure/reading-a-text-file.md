---
title:                "Lecture d'un fichier texte"
html_title:           "Arduino: Lecture d'un fichier texte"
simple_title:         "Lecture d'un fichier texte"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/clojure/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Quoi & Pourquoi?

Lire un fichier texte, c'est être capable d'extraire et d'utiliser des données stockées dans un format textuel standard. Les programmeurs le font pour manipuler, analyser, ou tout simplement afficher le contenu de ces fichiers.

## Comment Faire :

Voilà un exemple simple de lecture d'un fichier texte en Clojure. Supposons que vous voulez lire le fichier "monFichier.txt".

```Clojure
(defn lire-fichier [nom-fichier]
  (with-open [rdr (clojure.java.io/reader nom-fichier)]
    (slurp rdr)))
```

Si nous avons un fichier "monFichier.txt" qui contient "Bonjour le monde, je suis Clojure!", l'output sera:

```Clojure
(lire-fichier "monFichier.txt")

;; Output: 
"Bonjour le monde, je suis Clojure!"
```

## Immersion Profonde :

Lire un fichier texte est une des opérations de base en programmation. En fait, elle est si fondamentale que Clojure, comme plusieurs langages modernes, l'a intégré directement dans la syntaxe de base. C'est ce qui donne à "slurp" sa grande utilité.

Il existe des alternatifs à "slurp", comme "line-seq" qui lit le fichier ligne par ligne, ce qui peut être plus performant pour de grands fichiers.

```Clojure
(with-open [rdr (clojure.java.io/reader "monFichier.txt")]
  (doall (line-seq rdr))) ; retourne une séquence des lignes du fichier
```

L'intégration est facilitée par le Java sous-jacent de Clojure, utilisant des opérations de bas niveau pour lire les octets du fichier et les traduire en chaîne de caractères.

## Voir Aussi :

Pour aller plus loin, voici quelques liens pour approfondir le sujet :

* Documentation officielle Clojure sur les opérations de fichier : https://clojure.org/guides/io
* Tutoriel interactif Clojure : http://www.4clojure.com
* Questions courantes sur StackOverflow : https://stackoverflow.com/questions/tagged/clojure