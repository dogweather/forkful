---
title:                "Convertir une date en chaîne de caractères"
html_title:           "Gleam: Convertir une date en chaîne de caractères"
simple_title:         "Convertir une date en chaîne de caractères"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/clojure/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Qu'est-ce que c'est & Pourquoi?

Convertir une date en chaîne de caractères, en programmation, consiste à transformer une donnée de date en une série de caractères (string). Les développeurs le font souvent pour faciliter l'affichage ou la manipulation de dates.

## Comment faire:

En Clojure, la bibliothèque `clj-time` nous offre le moyen de convertir une date en chaîne de caractères. Voici un exemple de code:

```Clojure
(require '[clj-time.format :as f])
(def my-date (f/parse (f/formatter "MM-dd-yyyy") "03-24-2022"))
(f/unparse (f/formatters :date-time-no-ms) my-date)
```

Ce code prend une chaîne de caractères représentant une date, la convertit en le format de date de `clj-time`, puis la reconvertit en chaîne de caractères. 

## Plongée profonde: 

La conversion des dates en chaînes de caractères est une pratique courante en informatique. Historiquement, elle est employée pour afficher des dates de manière lisible pour l'utilisateur ou pour stocker des dates sous une forme normalisée dans les bases de données.

Il existe des alternatives à `clj-time`, comme la bibliothèque intégrée `java.time` de Java, mais `clj-time` offre une API plus Clojure-esque pour travailler avec les dates et le temps.

Côté implémentation, `clj-time` fait l'interface avec `Joda-Time`, une bibliothèque très respectée pour la manipulation des dates en Java. Elle offre une grande variété de convertisseurs de format et est considérée comme très fiable.

## Voir aussi:

[] Pour en savoir plus sur le traitement des dates et heures dans Clojure, consultez 'Clojure for the Brave and True': https://www.braveclojure.com/core-functions-in-depth/  
[] Pour une instance dédiée aux dates et heures en Java, voir 'Joda-Time': http://joda-time.sourceforge.net  
[] Pour une présentation détaillée de `clj-time`, visitez la page GitHub du projet: https://github.com/clj-time/clj-time