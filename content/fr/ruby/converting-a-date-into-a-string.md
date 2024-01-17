---
title:                "Conversion d'une date en chaîne de caractères"
html_title:           "Ruby: Conversion d'une date en chaîne de caractères"
simple_title:         "Conversion d'une date en chaîne de caractères"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/ruby/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Qu'est-ce que c'est et pourquoi le faire?
Convertir une date en chaîne de caractères est un processus que les programmeurs utilisent pour transformer une information de date en une représentation lisible pour les humains. Cela peut être utile pour présenter des données de manière plus compréhensible ou pour l'affichage dans un format spécifique.

## Comment faire:
Voici un exemple de code en Ruby pour convertir une date en chaîne de caractères:
```Ruby
date = Time.now.strftime("%d/%m/%Y")
puts date
```
La sortie de ce code sera la date actuelle formatée en jour/mois/année, soit « 17/10/2021 ».

## Plongée plus en profondeur:
Avant la popularité des langages de programmation modernes, les ordinateurs stockaient les dates sous forme de nombres complexes. Les programmeurs devaient donc utiliser des techniques de conversion pour les afficher de manière lisible pour les humains. De nos jours, il existe d'autres alternatives telles que les librairies et les frameworks qui permettent aux programmeurs de convertir facilement des dates en chaînes de caractères. L'implémentation de ces conversions peut varier en fonction du langage de programmation utilisé.

## Voir aussi:
- [Documentation officielle de Ruby pour convertir des dates en chaînes de caractères] (https://ruby-doc.org/core-3.0.0/Time.html#method-i-strftime)
- [Article sur les différentes façons de formater des dates dans Ruby] (https://advir.co/blog/manipulating-dates-in-ruby)