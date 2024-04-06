---
date: 2024-01-20 17:36:13.234403-07:00
description: "Comment faire : Historiquement, Clojure, une variante moderne de Lisp,\
  \ g\xE8re les dates via la biblioth\xE8que Java Joda-Time. Depuis Java 8, `java.time`,\
  \ un\u2026"
lastmod: '2024-04-05T22:51:11.414475-06:00'
model: gpt-4-1106-preview
summary: "Historiquement, Clojure, une variante moderne de Lisp, g\xE8re les dates\
  \ via la biblioth\xE8que Java Joda-Time."
title: "Conversion d'une date en cha\xEEne de caract\xE8res"
weight: 28
---

## Comment faire :
```clojure
;; Import clj-time pour le travail avec les dates
(require '[clj-time.format :as fmt])

;; Créer un formateur de date
(def formateur (fmt/formatters :basic-date-time))

;; Convertir un objet Joda Time en chaîne de caractères
(defn date-en-chaine [date]
  (fmt/unparse formateur date))

;; Exemple d'utilisation
(def ma-date (t/now))
(println (date-en-chaine ma-date))
```
Sortie:
```
"20230405T142920.000Z"
```

## Deep Dive
Historiquement, Clojure, une variante moderne de Lisp, gère les dates via la bibliothèque Java Joda-Time. Depuis Java 8, `java.time`, un package plus récent, est souvent utilisé.

Alternativement, la fonction `str` de Clojure peut convertir des dates, mais sans formatage. Il est préférable d'utiliser `clj-time` ou `java.time` pour un contrôle précis.

Concernant l'implémentation, `clj-time` s'appuie sur Joda-Time pour une API de date/heure complète et immuable contrairement à `java.util.Date`.

## Voir également
- Documentation de `clj-time`: [https://github.com/clj-time/clj-time](https://github.com/clj-time/clj-time)
- Guide Clojure pour `java.time`: [https://clojure.org/guides/deps_and_cli#_working_with_time](https://clojure.org/guides/deps_and_cli#_working_with_time)
- Un aperçu de Joda-Time: [https://www.joda.org/joda-time/](https://www.joda.org/joda-time/)
