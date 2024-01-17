---
title:                "Écrire un fichier texte"
html_title:           "Clojure: Écrire un fichier texte"
simple_title:         "Écrire un fichier texte"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/clojure/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Quoi & Pourquoi?

Ecrire un fichier texte est le fait de créer un document composé de caractères et de symboles pouvant être lu par un ordinateur. Les programmeurs le font pour stocker et transférer des données dans un format compréhensible pour les machines.

## Comment faire:

```Clojure
;; Pour écrire un fichier texte, il faut utiliser la fonction `spit` avec le nom du fichier et le contenu à écrire en paramètres.
(spit "mon_fichier.txt" "Bonjour le monde!")
;; Ceci va créer un fichier nommé "mon_fichier.txt" contenant le texte "Bonjour le monde!" 

;; On peut également utiliser une variable pour stocker le contenu à écrire.
(def mon_texte "Salut tout le monde!")
(spit "mon_fichier.txt" mon_texte)
;; Ceci va créer un fichier nommé "mon_fichier.txt" contenant le texte "Salut tout le monde!"

```

## Plongée en profondeur:

L'écriture de fichiers texte est un élément fondamental de la programmation, car elle permet de manipuler et de stocker des données à des fins diverses. Il existe des alternatives telles que l'utilisation de bases de données pour stocker des données, mais écrire des fichiers texte reste une méthode simple et efficace. L'implémentation de la fonction `spit` se fait en utilisant les fonctions de manipulation de fichiers de la bibliothèque standard de Clojure.

## Voir aussi:

- La documentation officielle de la fonction `spit`: <https://clojuredocs.org/clojure.core/spit>
- D'autres façons d'écrire des fichiers texte en Clojure: <http://clojure-doc.org/articles/cookbook/file_io.html>