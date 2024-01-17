---
title:                "Comparer deux dates"
html_title:           "Ruby: Comparer deux dates"
simple_title:         "Comparer deux dates"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/ruby/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Quoi & Pourquoi?

Comparer deux dates est un processus courant en programmation qui consiste à vérifier si une date donnée est antérieure, postérieure ou égale à une autre date. Les programmeurs utilisent cette fonction pour des besoins tels que le tri chronologique de données ou la validation de données entrées par les utilisateurs.

## Comment faire:

Voici deux manières de comparer des dates en Ruby:

```Ruby
#Méthode 1: Utiliser l'opérateur de comparaison ">"
date_1 = Date.new(2021, 7, 21) #date_1 est antérieure à date_2
date_2 = Date.new(2021, 7, 28)
puts date_1 > date_2 #affiche "false"

#Méthode 2: Utiliser la méthode "compare"
date_1 = Date.new(2021, 7, 21) #date_1 est postérieure à date_2
date_2 = Date.new(2021, 7, 15)
puts date_1.compare(date_2) #affiche "1"
```

## Plongée en profondeur:

Avant l'introduction de la classe Date en Ruby 1.8, la comparaison de dates était plus complexe. Les programmeurs devaient convertir les dates en nombres pour les comparer. Une alternative à la classe Date est la gem "chronic" qui permet de manipuler des dates sous forme de phrases en langage naturel. La comparaison de dates en Ruby se fait en se basant sur l'objet "Time" qui stocke la date et l'heure en nombre de secondes depuis le 1er janvier 1970.

## Voir aussi:

Pour en savoir plus sur la classe Date et ses méthodes, consultez la documentation officielle de Ruby: https://ruby-doc.org/stdlib-2.7.2/libdoc/date/rdoc/Date.html

La gem "chronic" est disponible sur RubyGems: https://rubygems.org/gems/chronic