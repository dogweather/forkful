---
title:                "Comparaison de deux dates"
html_title:           "Ruby: Comparaison de deux dates"
simple_title:         "Comparaison de deux dates"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/ruby/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Pourquoi
La comparaison de deux dates est un outil utile pour les développeurs Ruby lorsqu'ils ont besoin de déterminer si une date est antérieure, postérieure ou égale à une autre. Cela peut être particulièrement utile dans les applications de gestion de temps ou dans la manipulation de données de calendrier.

## Comment faire
Pour comparer deux dates en Ruby, vous pouvez utiliser la méthode `Date#<=>`, qui renvoie -1 si la première date est antérieure à la seconde, 0 si les deux dates sont égales et 1 si la première date est postérieure à la seconde.

Par exemple, pour comparer les dates d'aujourd'hui et de demain :

```Ruby
aujourdhui = Date.today
demain = Date.today + 1
aujourdhui <=> demain # renvoie -1
```

Vous pouvez également utiliser les opérateurs de comparaison `==`, `<`, `>`, `<=` et `>=` pour comparer deux dates. Cependant, cela ne fonctionnera que si les deux objets ont le même type (Date, Time, DateTime) et ne prend pas en compte la précision des secondes, contrairement à la méthode `Date#<=>`.

## Plongée en profondeur
Il est important de noter que si vous comparez une date avec un objet DateTime ou Time, la comparaison ne se fera pas correctement car DateTime et Time prennent en compte l'heure, les minutes et les secondes, tandis que Date ne prend en compte que la date. Pour comparer correctement des dates avec des objet DateTime ou Time, vous devez les convertir en objets Date en utilisant la méthode `Date#to_date`.

De plus, si vous utilisez des dates représentées en chaînes de caractères, assurez-vous de les parser en objets Date en utilisant la méthode `Date.parse` avant de les comparer.

## Voir aussi
- [Documentation officielle de Ruby sur la comparaison de dates](https://ruby-doc.org/stdlib/libdoc/date/rdoc/Date.html#method-i-3C-3D-3E)
- [Documentation officielle de Ruby sur les opérateurs de comparaison](https://ruby-doc.org/core-2.7.1/Comparable.html)