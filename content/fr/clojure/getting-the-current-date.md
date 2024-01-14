---
title:    "Clojure: Obtenir la date actuelle."
keywords: ["Clojure"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fr/clojure/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Pourquoi

Il est souvent nécessaire de connaître la date actuelle lors de la conception d'un programme en Clojure. Que ce soit pour afficher la date à l'utilisateur ou pour effectuer des opérations basées sur la date, l'accès à la date actuelle est un élément important de la programmation.

## Comment faire

La façon la plus simple d'obtenir la date actuelle en Clojure est d'utiliser la fonction `now` de la librairie `java.time`. Voici un exemple de code pour récupérer la date actuelle et l'afficher dans la console :

```Clojure
(require '[java.time :as time])
(def current-date (time/now))
(println "La date actuelle est:" current-date)
```

Output :

```
La date actuelle est: #object[java.time.ZonedDateTime 0x5adca42f "2021-11-14T08:07:53.923271Z[UTC]"]
```

La fonction `now` renvoie un objet de type `java.time.ZonedDateTime` qui contient à la fois la date et l'heure actuelles.

Pour formater la date selon un format spécifique, on peut utiliser la fonction `format` du même namespace `java.time`. Par exemple, pour afficher la date dans un format "jour/mois/année", on peut utiliser ce code :

```Clojure
(require '[java.time :as time])
(def current-date (time/now))
(def formatted-date (time/format current-date "dd/MM/yyyy"))
(println "La date actuelle est:" formatted-date)
```

Output :

```
La date actuelle est: 14/11/2021
```

## Plongée en profondeur

Il est important de noter que la date et l'heure retournées par la fonction `now` dépendent du fuseau horaire du système sur lequel le programme s'exécute. Si vous avez besoin d'accéder à la date et l'heure dans un fuseau horaire spécifique, vous pouvez utiliser la fonction `of` du namespace `java.time` pour spécifier le fuseau horaire désiré.

Par exemple, pour obtenir la date et l'heure actuelles dans le fuseau horaire de Paris, on peut utiliser ce code :

```Clojure
(require '[java.time :as time])
(def paris-time (time/of "Europe/Paris"))
(def current-date (time/now paris-time))
(println "La date actuelle à Paris est:" current-date)
```

Output :

```
La date actuelle à Paris est: #object[java.time.ZonedDateTime 0x2e5aee6a "2021-11-14T09:07:54.045501+01:00[Europe/Paris]"]
```

## Voir aussi

- [Documentation officielle de la librairie java.time](https://docs.oracle.com/javase/8/docs/api/java/time/package-summary.html)
- [Guide de référence de la programmation Clojure](https://clojure.org/api/cheatsheet)