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

## Pourquoi

Il est souvent nécessaire de comparer deux dates dans la programmation, que ce soit pour vérifier les horaires de rendez-vous, calculer des délais ou trier des événements chronologiquement. Dans cet article, nous allons explorer comment comparer deux dates en utilisant Clojure, un langage de programmation fonctionnel dynamique.

## Comment faire

Pour comparer deux dates en Clojure, nous pouvons utiliser la fonction `compare`, qui renvoie un entier représentant la relation entre les deux dates. Voici un exemple de code comparant deux dates :

```Clojure
(def date1 (java.util.Date. "01/07/2021")) ; crée une date au format mm/jj/aaaa
(def date2 (java.util.Date. "01/01/2021"))

(println (compare date1 date2)) ; renvoie 1, car date1 est après date2
```

Nous pouvons également utiliser les comparateurs `>` (plus grand), `<` (plus petit) et `=` (égal) pour comparer deux dates. Ces comparateurs fonctionnent avec des objets `java.util.Date` et `java.sql.Date`. Voici un exemple utilisant ces comparateurs pour trier une liste de dates :

```Clojure
(def dates [(java.util.Date. "01/07/2021")
            (java.util.Date. "01/01/2021")
            (java.util.Date. "01/03/2021")])

(sort < dates) ; renvoie une liste triée avec la date la plus ancienne en premier
```

La sortie pour ces deux exemples sera :

```
1
(01/01/2021, 01/03/2021, 01/07/2021)
```

## Plongée en profondeur

Lorsque nous comparons deux dates en utilisant la fonction `compare`, les résultats suivants peuvent être renvoyés :

- `-1` si la première date est avant la deuxième
- `0` si les deux dates sont égales
- `1` si la première date est après la deuxième.

Il est important de noter que les comparaisons de dates ne prennent pas en compte l'heure. Si nous voulons aussi comparer l'heure, nous devons utiliser un objet `java.sql.Timestamp` qui stocke également les heures et les minutes.

De plus, lorsque nous utilisons des comparateurs comme `>` ou `<`, il est important de noter que tous les objets `Date` sont convertis en millisecondes depuis le 1er janvier 1970 avant d'être comparés. Cela signifie que si nous comparons des objets avec des millisecondes différentes, les résultats peuvent être surprenants.

## Voir aussi

- [Documentation de Clojure sur les comparateurs](https://clojuredocs.org/clojure.core/<)
- [Tutoriel sur les comparaisons de dates en Clojure](https://purelyfunctional.org/compare-dates-in-clojure-without-time-timezones/)