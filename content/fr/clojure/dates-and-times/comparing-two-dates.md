---
date: 2024-01-20 17:32:26.165810-07:00
description: "Comparer deux dates, c'est \xE9valuer leur ordre chronologique relatif.\
  \ Les d\xE9veloppeurs le font pour trier des \xE9v\xE9nements, g\xE9rer des d\xE9\
  lais ou v\xE9rifier des\u2026"
lastmod: '2024-03-13T22:44:57.295250-06:00'
model: gpt-4-1106-preview
summary: "Comparer deux dates, c'est \xE9valuer leur ordre chronologique relatif.\
  \ Les d\xE9veloppeurs le font pour trier des \xE9v\xE9nements, g\xE9rer des d\xE9\
  lais ou v\xE9rifier des\u2026"
title: Comparer deux dates
weight: 27
---

## Quoi et Pourquoi ?
Comparer deux dates, c'est évaluer leur ordre chronologique relatif. Les développeurs le font pour trier des événements, gérer des délais ou vérifier des périodes de validité.

## Comment faire :
Voici quelques exemples simples pour comparer des dates en Clojure :

```Clojure
;; Charger la librairie clj-time
(require '[clj-time.core :as time])
(require '[clj-time.coerce :as coerce])

;; Créer deux dates pour la comparaison
(def date1 (time/date-time 2023 3 25))
(def date2 (time/date-time 2023 4 2))

;; Comparer les dates
(time/before? date1 date2)      ;; => true : date1 est avant date2
(time/after? date1 date2)       ;; => false : date1 n'est pas après date2
(= date1 date2)                 ;; => false : date1 et date2 ne sont pas égales

;; Calculer la différence entre deux dates
(def duration (time/interval date1 date2))
(time/in-days duration)         ;; => 8 : différence en jours
```

## Exploration détaillée
Historiquement, les opérations sur les dates en Clojure utilisaient la bibliothèque standard Java `java.util.Date`, mais cette approche avait des limitations. Pour offrir de meilleures fonctionnalités, `clj-time`, une wrapper autour de Joda Time, a été introduite, et est devenue populaire avant l'introduction de `java.time` dans Java 8.

Les alternatives incluent l'usage direct de `java.time` pour les opérations sur les dates. Néanmoins, `clj-time` reste utilisé en raison de sa simplicité et de son intégration avec Clojure.

Pour comparer deux dates avec `clj-time`, on se sert de fonctions comme `before?`, `after?` et `equal?`. Ces fonctions comparent des objets `DateTime`, qui peuvent être créés avec `date-time` ou convertis à partir d'autres formats via `coerce`.

## Voir également
- Pour en savoir plus sur `clj-time`, regardez [clj-time GitHub Repo](https://github.com/clj-time/clj-time).
- La documentation officielle de [Clojure](https://clojure.org/).
- Un guide supplémentaire pour `java.time`: [Java 8 Date Time API](https://docs.oracle.com/javase/tutorial/datetime/).
