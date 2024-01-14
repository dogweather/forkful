---
title:                "Clojure: Obtenir la date actuelle"
simple_title:         "Obtenir la date actuelle"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/clojure/getting-the-current-date.md"
---

{{< edit_this_page >}}

# Pourquoi

La récupération de la date actuelle est une tâche courante dans la programmation Clojure. Elle peut être utile pour afficher la date dans une interface utilisateur ou pour effectuer des calculs basés sur le temps.

# Comment faire

Pour obtenir la date actuelle en Clojure, nous pouvons utiliser la fonction `now` du package `java.time.LocalDateTime`.

```Clojure
(import [java.time.LocalDateTime])

(def date (LocalDateTime/now))
```

La variable `date` contiendra maintenant un objet de type `java.time.LocalDateTime` représentant la date et l'heure actuelles.

Pour afficher la date dans un format spécifique, nous pouvons utiliser la fonction `format` du package `java.time.format.DateTimeFormatter`.

```Clojure
(import [java.time.format.DateTimeFormatter])

(def formatter (DateTimeFormatter/ofPattern "dd/MM/yyyy"))

(def date-formatted (LocalDateTime/format date formatter))

(println date-formatted)
;; Output: 14/10/2021
```

Nous pouvons également obtenir la date dans un fuseau horaire spécifique en utilisant la méthode `atZone` du package `java.time.LocalDateTime` et en spécifiant le fuseau horaire souhaité.

```Clojure
(def zone-id (java.time.ZoneId/of "Europe/Paris"))

(def date-paris (LocalDateTime/atZone date zone-id))

(println date-paris)
;; Output: 2021-10-14T10:00+02:00[Europe/Paris]
```

# Plongée en profondeur

Pour comprendre comment la fonction `now` fonctionne, nous pouvons examiner le code source de la classe `LocalDateTime` dans le package `java.time`.

Nous pouvons voir que `now` utilise la méthode statique `now()` de la classe `System` pour obtenir l'heure système en millisecondes, puis utilise la classe `java.time.Instant` pour créer un objet `Instant` représentant cette heure.

Ensuite, la méthode `ofInstant` est utilisée pour créer un objet `LocalDateTime` à partir de l'objet `Instant`.

# Voir aussi

- [Documentation de `java.time.LocalDateTime`](https://docs.oracle.com/javase/8/docs/api/java/time/LocalDateTime.html)
- [Documentation de `java.time.format.DateTimeFormatter`](https://docs.oracle.com/javase/8/docs/api/java/time/format/DateTimeFormatter.html)
- [Documentation de `java.time.ZoneId`](https://docs.oracle.com/javase/8/docs/api/java/time/ZoneId.html)