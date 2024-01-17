---
title:                "Obtenir la date actuelle"
html_title:           "Clojure: Obtenir la date actuelle"
simple_title:         "Obtenir la date actuelle"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/clojure/getting-the-current-date.md"
---

{{< edit_this_page >}}

Quelle et pourquoi?

Obtenir la date actuelle est une fonctionnalité essentielle dans de nombreux programmes informatiques. Les programmeurs utilisent cette fonction pour enregistrer la date et l'heure d'un événement ou pour effectuer des calculs liés au temps.

## Comment faire:

```Clojure
(require '[java.time.LocalDate :as time])

;; Obtenez la date actuelle
(def current-date (time/local-date))

;; Obtenez l'année actuelle
(def current-year (time/year current-date))

;; Obtenez le mois actuel
(def current-month (time/month current-date))

;; Obtenez le jour actuel
(def current-day (time/day-of-month current-date))
```
Le code ci-dessus utilise la bibliothèque intégrée java.time pour obtenir la date actuelle et extraire des informations telles que l'année, le mois et le jour.

Exemple de sortie:

```
current-date => #object[java.time.LocalDate 0x224e76fd 2021-10-13]
current-year => 2021
current-month => #object[java.time.Month 0x5551fa84 OCTOBER]
current-day => 13
```

## Plongée en profondeur

Avant l'avènement de la bibliothèque java.time, les programmeurs utilisaient la classe Date de Java pour obtenir la date actuelle. Cependant, cette classe était sujette à des problèmes de performance et de fiabilité. La bibliothèque java.time a été introduite dans Java 8 pour résoudre ces problèmes et fournir une interface plus conviviale pour travailler avec les dates et heures.

Alternatives:

Si vous utilisez une version antérieure de Java qui n'inclut pas la bibliothèque java.time, vous pouvez utiliser la bibliothèque open-source clj-time pour obtenir la date actuelle. Cette bibliothèque fournit des fonctions similaires à celles de java.time mais avec une syntaxe différente.

Détails de mise en œuvre:

La fonction `time/local-date` renvoie une instance de la classe `java.time.LocalDate` qui représente la date actuelle. Ensuite, on peut utiliser les méthodes `year`, `month` et `day-of-month` pour extraire des informations spécifiques de cette date.

## Voir aussi

- Documentation officielle Clojure: https://clojure.org/
- Bibliothèque java.time: https://docs.oracle.com/javase/8/docs/api/java/time/package-summary.html
- Bibliothèque clj-time: https://github.com/clj-time/clj-time