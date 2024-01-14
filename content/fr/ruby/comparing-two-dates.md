---
title:    "Ruby: Comparaison de deux dates"
keywords: ["Ruby"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fr/ruby/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Pourquoi

La comparaison de deux dates peut sembler être une tâche simple en apparence, mais elle peut être très utile dans de nombreux scénarios de programmation en Ruby. Par exemple, vous pourriez avoir besoin de vérifier si une date est avant ou après une autre, ou encore comparer des dates pour trier une liste d'objets. Dans cet article, nous allons explorer différentes façons de comparer des dates en utilisant Ruby.

## Comment faire

Pour comparer deux dates en Ruby, nous pouvons utiliser la méthode `difference` de la classe `Date`. Cette méthode renvoie un objet `Date`, représentant la différence entre deux dates. Voyons un exemple concret :

```
Ruby
require 'date'

date1 = Date.new(2020, 5, 20)
date2 = Date.new(2020, 5, 10)

difference = date1.difference(date2)
puts difference #=> 10
```

Dans cet exemple, nous créons deux objets `Date` et utilisons la méthode `difference` pour calculer le nombre de jours entre les deux dates. La sortie sera `10`, car il y a dix jours d'écart entre le 10 et le 20 mai.

Nous pouvons également utiliser la méthode `<=` pour vérifier si une date est avant ou après une autre. Par exemple :

```
Ruby
require 'date'

date1 = Date.new(2020, 5, 10)
date2 = Date.new(2020, 5, 20)

if date1 <= date2
  puts "La date 1 est avant la date 2"
else
  puts "La date 2 est avant la date 1"
end

#=> La date 1 est avant la date 2
```

Dans cet exemple, nous utilisons l'opérateur de comparaison `<=` pour vérifier si la date 1 est avant la date 2. Comme c'est le cas, la condition est vraie et le message "La date 1 est avant la date 2" sera affiché.

## Deep Dive

En utilisant la méthode `difference` pour calculer la différence entre deux dates, il est important de noter que le résultat sera en jours, et non en mois ou en années. Pour obtenir une différence en mois ou en années, nous pouvons utiliser la méthode `month` et `year`, respectivement.

```
Ruby
require 'date'

date1 = Date.new(2020, 5, 10)
date2 = Date.new(2021, 5, 10)

years_diff = date2.year - date1.year
puts "Années de différence : #{years_diff}" #=> Années de différence : 1

months_diff = date2.month - date1.month
puts "Mois de différence : #{months_diff}" #=> Mois de différence : 0
```

En utilisant ces méthodes, nous pouvons calculer la différence en années ou en mois entre deux dates. Gardez à l'esprit que la méthode `month` renvoie le numéro du mois, donc si vous voulez obtenir un résultat lisible comme "2 mois de différence", vous devrez peut-être effectuer un peu plus de calcul.

## Voir aussi

- [Documentation Ruby sur les dates](https://ruby-doc.org/stdlib/libdoc/date/rdoc/Date.html)
- [Tutoriel sur les dates en Ruby](https://www.tutorialspoint.com/ruby/ruby_date_time.htm)