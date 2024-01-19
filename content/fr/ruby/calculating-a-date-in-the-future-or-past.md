---
title:                "Calculer une date dans le futur ou le passé"
html_title:           "Ruby: Calculer une date dans le futur ou le passé"
simple_title:         "Calculer une date dans le futur ou le passé"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/ruby/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Quoi & Pourquoi?
Calculer une date dans le futur ou le passé signifie déterminer une date spécifique en ajoutant ou en soustrayant un certain temps à une date donnée. Les programmeurs le font pour gérer des tâches comme le suivi du temps, les rendez-vous, les alertes et autres opérations similaires.

## Comment faire :
Ruby offre une manière simple d'ajouter ou de soustraire des dates. Considérez l'exemple suivant:

```ruby
require 'date'

# Aujourd'hui
aujourdhui = Date.today

# Dans le futur
future = aujourdhui + 30

# Dans le passé
passe = aujourdhui - 30

puts "Aujourd'hui : #{aujourdhui}"
puts "Dans 30 jours : #{future}"
puts "Il y a 30 jours : #{passe}"
```

Dans cet exemple, vous constaterez que nous manipulons la date de manière à calculer les dates dans le futur (`future`) et dans le passé (`passe`). Nous ajoutons ou soustrayons le nombre de jours à la date d'aujourd'hui.

## Plongée en profondeur
Historiquement, les opérations sur les dates étaient assez compliquées en raison de la diversité des calendriers et des calculs impliqués. Ruby simplifie cela en encapsulant ces détails derrière la classe `Date` et ses méthodes associées.

Vous pouvez aussi utiliser d'autres bibliothèques comme `ActiveSupport::TimeWithZone` ou `Time` pour d'autres manipulations temporaires. 

Ruby stocke les dates sous forme de Gregorian Julian Day Number, une mesure du nombre de jours passés depuis le 1er janvier 4713 av. J-C. C'est pourquoi on peut facilement ajouter ou soustraire des jours à partir d'une date donnée.

## Voir aussi
Prenez le temps de consulter la documentation Ruby sur les dates pour obtenir plus d'informations et d'exemples : [https://ruby-doc.org/stdlib-2.5.1/libdoc/date/rdoc/Date.html](https://ruby-doc.org/stdlib-2.5.1/libdoc/date/rdoc/Date.html)

Aussi, pour une compréhension plus profonde de la manipulation des dates et des heures dans Ruby, je vous recommande ce guide complet par Daniel Kehoe : [https://learn.co/lessons/ruby-intro-to-enumerables](https://learn.co/lessons/ruby-intro-to-enumerables). 

Enjoy your coding!