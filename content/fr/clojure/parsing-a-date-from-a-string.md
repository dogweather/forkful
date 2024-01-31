---
title:                "Analyse d'une date à partir d'une chaîne de caractères"
date:                  2024-01-20T15:35:28.178511-07:00
html_title:           "Arduino: Analyse d'une date à partir d'une chaîne de caractères"
simple_title:         "Analyse d'une date à partir d'une chaîne de caractères"

category:             "Clojure"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/clojure/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## What & Why?
(Quoi et Pourquoi ?)
Transformer une date en chaîne de caractères en objet de date est essentiel pour manipuler et comparer les dates. Les développeurs font ça parce que les dates viennent souvent sous forme de texte depuis des bases de données, fichiers ou l'Internet.

## How to:
(Comment faire :)
Pour parser une date en Clojure, on utilise la librairie `clj-time`, qui est un wrapper de Joda-Time. Voici un exemple :

```Clojure
(require '[clj-time.format :as fmt])
(require '[clj-time.coerce :as coerce])

;; Pour définir un format de date
(def date-format (fmt/formatters :basic-date-time))

;; Pour parser une chaîne de caractères en objet Joda-Time
(def date-string "20230401T123456Z")
(def parsed-date (fmt/parse date-format date-string))

;; Pour convertir l'objet Joda-Time en objet java.util.Date
(def date-as-java-date (coerce/to-date parsed-date))

;; Afficher l'objet Date
(println date-as-java-date)
```

Sample output:
```
Sat Apr 01 12:34:56 UTC 2023
```
## Deep Dive
(Plongée en profondeur)
Clj-time est basé sur Joda-Time, une bibliothèque de gestion du temps robuste avant l'introduction de `java.time` dans Java 8. Une alternative moderne est d'utiliser `java.time` directement avec Java interop. Clj-time offre une abstraction qui rend le code plus idiomatique à Clojure.

Implémentation : en interne, la fonction `parse` de clj-time utilise un `DateTimeFormatter` de Joda-Time pour transformer la chaîne en objet `DateTime`. L'étape de `coerce` est nécessaire pour utiliser l'objet date avec des bibliothèques Java qui attendent `java.util.Date`.

## See Also
(Voir aussi)
- [clj-time GitHub repository](https://github.com/clj-time/clj-time)
- [Clojure Java Time, une alternative plus récente](https://github.com/dm3/clojure.java-time)
- [Java Time Guide](https://www.baeldung.com/java-8-date-time-intro) pour les opérations de date/timestamp en interop.
