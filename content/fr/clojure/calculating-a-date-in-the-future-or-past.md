---
title:                "Calculer une date dans le futur ou le passé"
html_title:           "Clojure: Calculer une date dans le futur ou le passé"
simple_title:         "Calculer une date dans le futur ou le passé"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/clojure/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Pourquoi

Si vous avez déjà eu besoin de calculer une date dans le futur ou dans le passé, vous savez que cela peut être une tâche fastidieuse. Heureusement, Clojure a une solution simple et élégante pour cela.

## Comment Faire

```Clojure
; Pour obtenir la date d'aujourd'hui
(println (java.util.Date.))

; Pour calculer une date dans le futur ou dans le passé, nous allons utiliser la fonction
'java.util.Calendar' de Java.

; Pour cela, nous devons d'abord importer la librairie Java dans notre code Clojure :
(import '(java.util Calendar))

; Ensuite, nous pouvons créer une instance de la classe Calendar et utiliser la méthode 'add' pour ajouter ou soustraire des jours, mois ou années à la date actuelle :
(def today (Calendar/getInstance))

; Pour ajouter 3 jours à la date actuelle :
(.add today Calendar/DAY_OF_MONTH 3)

; Pour soustraire 1 mois et 2 années :
(.add today Calendar/MONTH -1)
(.add today Calendar/YEAR -2)

; La méthode 'getTime' nous permet d'obtenir la date résultat sous forme de timestamp :
(.getTime today)
```

## Deep Dive

En utilisant la classe Calendar de Java, nous pouvons également spécifier une date de départ différente de la date actuelle, en utilisant la méthode 'set' au lieu de 'getInstance'.

De plus, en utilisant les constantes telles que 'Calendar/DAY_OF_MONTH' ou 'Calendar/MONTH', nous pouvons facilement préciser quelle partie de la date doit être modifiée.

Enfin, il est également possible d'utiliser la librairie 'clj-time', qui fournit des fonctions plus abstraites et faciles à utiliser pour manipuler les dates en Clojure.

## Voir Aussi

- [Documentation officielle de la classe Calendar](https://docs.oracle.com/javase/7/docs/api/java/util/Calendar.html)
- [Documentation de la librairie clj-time](https://github.com/clj-time/clj-time)