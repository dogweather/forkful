---
title:                "Comparer deux dates"
html_title:           "Clojure: Comparer deux dates"
simple_title:         "Comparer deux dates"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/ruby/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Quoi et Pourquoi?

La comparaison entre deux dates est l'opération où l'on détermine si une date est ultérieure, antérieure ou identique à une autre. Les programmeurs l'utilisent souvent pour effectuer des tâches, comme trier des événements par date ou déterminer des intervalles de temps.

## Comment faire:

Voici un exemple simple sur comment comparer deux dates en utilisant Ruby:

```Ruby
require 'date'

date1 = Date.new(2020, 12, 10)
date2 = Date.new(2021, 12, 10)

if date1 > date2
  puts "La date1 est supérieure à la date2"
elsif date1 < date2
  puts "La date1 est inférieure à la date2"
else
  puts "Les deux dates sont identiques"
end
```

Ce programme affichera "La date1 est inférieure à la date2".

## Exploration en profondeur

Historiquement, le langage Ruby a été conçu pour rendre la programmation plus agréable pour les développeurs. Il offre des outils de manipulation de dates plus faciles par rapport à d'autres langages. 

Parmi les alternatives, nous pouvons citer la bibliothèque 'Time' qui offre des fonctions similaires. Cependant, 'Date' offre des avantages en terme de simplicité lors du traitement des dates uniquement. 

La comparaison en Ruby se fait à travers le surchargement d'opérateurs, permettant de comparer directement deux instances de la classe 'Date'. Les détails de mise en œuvre sont cachés aux utilisateurs, pour rendre le code plus lisible et plus facile à écrire.

## Voir Aussi

Voici quelques ressources supplémentaires pour vous aider à vous familiariser davantage avec la manipulation de dates en Ruby:

1. Documentation officielle de la classe Date: https://ruby-doc.org/stdlib-3.0.0/libdoc/date/rdoc/Date.html
2. Tutoriel sur le module Time: https://www.tutorialspoint.com/ruby/ruby_date_time.htm
3. Article sur la gestion des dates et heures en Ruby: https://www.justinweiss.com/articles/3-key-takeaways-from-the-time-and-space-talk/