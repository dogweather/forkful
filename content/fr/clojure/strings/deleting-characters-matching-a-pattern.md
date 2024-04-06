---
date: 2024-01-20 17:42:06.420847-07:00
description: "How to: Pour supprimer des caract\xE8res qui correspondent \xE0 un motif\
  \ en Clojure, utilisez les fonctions `clojure.string/replace` et `re-pattern` ."
lastmod: '2024-04-05T21:53:58.846028-06:00'
model: gpt-4-1106-preview
summary: "Pour supprimer des caract\xE8res qui correspondent \xE0 un motif en Clojure,\
  \ utilisez les fonctions `clojure.string/replace` et `re-pattern` ."
title: "Suppression de caract\xE8res correspondant \xE0 un motif"
weight: 5
---

## How to:
Pour supprimer des caractères qui correspondent à un motif en Clojure, utilisez les fonctions `clojure.string/replace` et `re-pattern` :

```clojure
(require '[clojure.string :as str])

;; Supprimer tous les chiffres d'une chaîne
(defn delete-digits [input]
  (str/replace input (re-pattern "\\d") ""))

(delete-digits "J'ai 30 ans et je mesure 1m80.")
;; Résultat : "J'ai  ans et je mesure m."
```

Suppression grâce à une classe de caractères :

```clojure
;; Supprimer toutes les voyelles d'une chaîne
(defn delete-vowels [input]
  (str/replace input (re-pattern "[aeiouyAEIOUY]") ""))

(delete-vowels "Bonjour, comment ça va?")
;; Résultat : "Bnjr, cmmt ç v?"
```

## Deep Dive
Clojure, étant une JVM language, repose sur la puissance de Java pour les expressions régulières. Historiquement, les regex sont utilisées depuis les premiers jours de l'informatique et ont évolué avec des langages comme Perl. Clojure rend ces opérations concises et fonctionnelles.

Alternatives pour supprimer des motifs inclut `filter`, combiné avec `not` et `every?` pour un contrôle plus fin :

```clojure
(defn delete-if [pred s]
  (apply str (filter (comp not pred) s)))

(delete-if #(Character/isDigit %) "Eliminez les chiffres 123")
;; Résultat : "Eliminez les chiffres "
```

Niveau implémentation, `clojure.string/replace` utilise `java.lang.String.replaceAll` ou `java.lang.String.replace` en interne, en basant sur si le pattern est une regex ou non.

## See Also
Pour approfondir les regex en Clojure :

- Clojure Documentation on Strings: https://clojure.org/guides/learn/strings
- Java Regex Documentation: https://docs.oracle.com/javase/8/docs/api/java/util/regex/Pattern.html

Pour la manipulation des chaînes de caractères en Clojure :

- Clojure Cheatsheet, String Section: https://clojure.org/api/cheatsheet
