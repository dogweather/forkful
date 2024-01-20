---
title:                "Obtenir la date actuelle"
html_title:           "Bash: Obtenir la date actuelle"
simple_title:         "Obtenir la date actuelle"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/clojure/getting-the-current-date.md"
---

{{< edit_this_page >}}

# L'obtention de la date actuelle en Clojure

## Quoi & Pourquoi?
L'obtention de la date actuelle est une tâche qui récupère la date et l'heure à l'instant T. Les programmeurs font cela pour enregistrer quand un événement se produit, pour suivre le temps, ou pour comparer des dates.

## Comment faire :
En Clojure, on utilise `java.util.Date` pour obtenir la date actuelle.

```Clojure
(import 'java.util.Date)
(defn courant-date []
  (Date.))
```

Pour voir le résultat, appelez simplement la fonction:

```Clojure
(courant-date)
```

Le code va renvoyer la date et l'heure actuelles en fonction du système sur lequel il est exécuté.

## Approfondissement

Historiquement, en Clojure, l'obtention de la date actuelle a toujours été effectuée en utilisant la bibliothèque Java intégrée. Cependant, il existe une alternative plus moderne: l'utilisation de la bibliothèque clj-time.

```Clojure
(require '[clj-time.core :as time])

(defn courant-date []
  (time/now))
```

L'avantage de clj-time est qu'il offre une API plus riche et plus conviviale que java.util.Date, surtout pour les manipulations de date complexes. 

Il est important de noter que clj-time, comme java.util.Date, utilise le temps de l'horloge système pour obtenir la date. Ceci est normalement suffisant, mais peut poser des problèmes si l'horloge système est modifiée pendant l'exécution du programme.

## Voir Aussi:

1. Documentation officielle de Clojure: https://clojure.org/
2. Bibliothèque clj-time : https://github.com/clj-time/clj-time
3. Guide Java Date : https://www.javatpoint.com/java-date
4. Post sur StackOverflow sur l'utilisation de java.util.Date : https://stackoverflow.com/questions/5270225/working-with-date-time-in-clojure