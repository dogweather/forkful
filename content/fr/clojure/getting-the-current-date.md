---
title:                "Clojure: Obtenir la date actuelle"
programming_language: "Clojure"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/clojure/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Pourquoi

Si vous êtes un développeur Clojure, vous savez probablement qu'il est essentiel de pouvoir interagir avec les dates dans votre code. Que vous manipuliez des données ou que vous créiez des applications en temps réel, il est souvent nécessaire d'obtenir la date et l'heure actuelles. Dans cet article, nous allons expliquer comment faire cela en utilisant Clojure.

## Comment faire

Pour obtenir la date et l'heure actuelles en Clojure, vous pouvez utiliser la fonction `now` du module `java.time.Clock`. Cette fonction renvoie un objet `java.time.Instant` représentant la date et l'heure actuelles. Voici un exemple de code montrant comment utiliser cette fonction :

```Clojure
(import [java.time Clock])

(def current-date (Clock/now))

(println current-date)
```

Lorsque vous exécutez ce code, vous verrez la date et l'heure actuelles imprimées dans la console :

```
2019-09-30T15:22:25.370Z
```

Vous pouvez également utiliser la fonction `zoneOffset` pour obtenir le décalage de fuseau horaire actuel :

```Clojure
(def current-offset (.getOffset (Clock/now)))
```

Enfin, vous pouvez utiliser la fonction `LocalDate.now` du module `java.time` pour obtenir uniquement la date actuelle :

```Clojure
(import [java.time LocalDate])

(def current-date-only (LocalDate/now))

(println current-date-only)
```

La sortie de ce code sera simplement la date actuelle, sans l'heure :

```
2019-09-30
```

## Deep Dive

Si vous souhaitez en savoir plus sur les différents formats de date et d'heure en Clojure, vous pouvez également utiliser la fonction `format` du module `java.time.format.DateTimeFormatter`. Cette fonction vous permet de formater votre date et votre heure selon vos préférences. Voici un exemple de code montrant comment utiliser cette fonction pour afficher la date actuelle au format "jour/mois/année" :

```Clojure
(import [java.time.format DateTimeFormatter])

(def current-date-formatted (.format DateTimeFormatter/ofPattern "dd/MM/yyyy" (LocalDate/now)))

(println current-date-formatted)
```

La sortie de ce code sera la date actuelle au format "jour/mois/année" :

```
30/09/2019
```

N'hésitez pas à explorer les différentes options de format disponibles dans le module `java.time.format.DateTimeFormatter` pour personnaliser encore plus vos dates et heures.

## Voir aussi

Pour en savoir plus sur les dates en Clojure, vous pouvez consulter les liens suivants :

- [Documentation officielle Clojure sur les dates et heures](https://clojure.org/reference/java_interop#Date_and_Time)
- [Tutoriel YouTube sur l'utilisation des dates en Clojure](https://www.youtube.com/watch?v=PjclN7IyKsQ)
- [Article Medium présentant plusieurs façons d'obtenir la date et l'heure actuelles en Clojure](https://medium.com/clojurians/3-ways-to-get-the-current-date-and-time-in-clojure-b28e08850df6)