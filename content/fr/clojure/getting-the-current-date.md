---
title:                "Obtenir la date actuelle"
date:                  2024-01-20T15:13:56.287247-07:00
html_title:           "C: Obtenir la date actuelle"
simple_title:         "Obtenir la date actuelle"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/clojure/getting-the-current-date.md"
---

{{< edit_this_page >}}

## What & Why?
Obtenir la date actuelle, c'est saisir l'instant présent en code. Les développeurs s'en servent pour des journaux, des timestamps ou la gestion d'événements temporels. 

## How to:
Clojure utilise `java.util.Date` et la bibliothèque `clj-time` pour gérer les dates. Voilà comment capturer le moment :

```clojure
;; Avec java.util.Date
(import 'java.util.Date)
(str (Date.))

;; Avec clj-time
(require '[clj-time.core :as time])
(require '[clj-time.format :as fmt])
(str (time/now))
(fmt/unparse (fmt/formatters :basic-date-time) (time/now))
```

Exemple de sortie :

```
"Tue Mar 21 14:52:03 EDT 2021"
"2021-03-21T14:52:03.123Z"
```

## Deep Dive
Clojure, étant une JVM language, puise dans les ressources de Java, comme `java.util.Date` depuis 1995. C'est utile mais vieux jeu; vous n'avez pas toutes les fonctionnalités modernes. 

La bibliothèque `clj-time` est une façade Clojure pour Joda-Time, une meilleure prise en main des concepts de temps. Avec `clj-time`, vous formatez, parsez, et manipulez les dates avec facilité. 

`java.time`, présent depuis Java 8, est une autre option robuste à considérer, malgré qu'elle soit moins idiomatique à Clojure sans couche d'abstraction.

## See Also
- Clojure `clj-time` bibliothèque: [https://github.com/clj-time/clj-time](https://github.com/clj-time/clj-time)
- Java `Date` documentation: [https://docs.oracle.com/javase/7/docs/api/java/util/Date.html](https://docs.oracle.com/javase/7/docs/api/java/util/Date.html)
- Joda-Time: [https://www.joda.org/joda-time/](https://www.joda.org/joda-time/)
- Java `java.time` package (Java 8+): [https://docs.oracle.com/javase/8/docs/api/java/time/package-summary.html](https://docs.oracle.com/javase/8/docs/api/java/time/package-summary.html)
