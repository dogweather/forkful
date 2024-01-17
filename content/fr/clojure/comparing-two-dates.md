---
title:                "Comparer deux dates"
html_title:           "Clojure: Comparer deux dates"
simple_title:         "Comparer deux dates"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/clojure/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Quoi et pourquoi?

Comparer deux dates est un processus courant en programmation pour déterminer si une date est antérieure, postérieure ou égale à une autre date. Ce type de comparaison est utile pour trier et filtrer des données, ainsi que pour créer des conditions logiques dans un programme.

## Comment faire:

Voici un exemple de code en Clojure pour comparer deux dates. Dans cet exemple, nous utilisons la fonction ```clojure >``` pour déterminer si la date 1 est postérieure à la date 2:

```Clojure
(def date1 (java.util.Date. 1990 1 1))
(def date2 (java.util.Date. 1995 1 1))

(> date1 date2)

; Output: false
```

Nous pouvons également utiliser la fonction ```clojure <``` pour déterminer si la date 1 est antérieure à la date 2:

```Clojure
(< date1 date2)

; Output: true
```

Pour vérifier si les deux dates sont égales, nous pouvons utiliser la fonction ```clojure =```

```Clojure
(= date1 date2)

; Output: false
```

## Plongeon profond:

La comparaison de dates est un concept commun en programmation et est utilisée dans de nombreux langages de programmation. Les langages orientés objets, tels que Java, utilisent souvent des méthodes spéciales pour comparer des objets de type date. Dans Clojure, nous utilisons les fonctions ```clojure >```, ```clojure <``` et ```clojure =``` pour effectuer des comparaisons de dates.

Une alternative à la comparaison de dates est d'utiliser des timestamps, qui représentent le temps en nombre de secondes écoulées depuis une date de référence. Les timestamps sont plus précis pour des comparaisons de dates précises.

## Voir aussi:

- Documentation officielle sur les fonctions de comparaison de Clojure: https://clojure.org/reference/data_structures#Comparison
- Un tutoriel sur la manipulation de dates en Clojure: https://www.tutorialspoint.com/clojure/clojure_date_time.htm
- Une discussion sur les avantages et les inconvénients d'utiliser des timestamps pour comparer des dates: https://stackoverflow.com/questions/5420385/timestamp-vs-date-as-primary-key-in-rdbms