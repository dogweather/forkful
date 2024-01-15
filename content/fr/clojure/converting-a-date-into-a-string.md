---
title:                "Convertir une date en chaîne de caractères"
html_title:           "Clojure: Convertir une date en chaîne de caractères"
simple_title:         "Convertir une date en chaîne de caractères"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/clojure/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Pourquoi

Vous pouvez avoir besoin de convertir une date en chaîne de caractères pour l'afficher dans un format spécifique ou pour l'utiliser dans une opération de comparaison. Cela peut être utile pour les applications de traitement de données, les tests ou la génération de rapports.

## Comment faire

```Clojure
(require '[java.time :as time])
;; importe la librairie Java.time pour manipuler les dates

(time/format (java.time.LocalDate/Now) "dd-MM-yyyy")
;; convertit la date actuelle en format "jour-mois-année"
;; sortie: "26-08-2021"

(time/format (java.time.LocalDateTime/Now) "dd/MM/yy HH:mm")
;; convertit la date et l'heure actuelles en format "jour/mois/année heure:minute"
;; sortie: "26/08/21 13:50"
```

## Plongée en profondeur

Clojure a une intégration étroite avec la librairie Java.time, ce qui permet une manipulation facile et précise des dates et des heures. Vous pouvez également utiliser des modèles de formatage spécifiques pour personnaliser la sortie de votre chaîne de caractères. De plus, vous pouvez utiliser des fonctions de comparaison telles que `before?` et `after?` pour comparer des dates converties en chaînes.

## Voir aussi

- [La documentation officielle de Java.time](https://docs.oracle.com/en/java/javase/11/docs/api/java.base/java/time/package-summary.html)
- [Un tutoriel sur la manipulation des dates en Clojure](https://github.com/clojure-cookbook/clojure-cookbook/blob/master/09_datesandtimes/9-12_manipulating-dates.asciidoc)