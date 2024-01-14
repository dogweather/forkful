---
title:                "Ruby: Comparaison de deux dates"
programming_language: "Ruby"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/ruby/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Pourquoi

Comparaison de dates est une tâche courante en programmation, particulièrement en Ruby. Cela permet de vérifier si une date est avant ou après une autre, ou si elles sont égales. Comprendre comment comparer des dates en Ruby peut améliorer considérablement la qualité de votre code et vous faire gagner du temps.

## Comment faire 

Pour comparer deux dates en Ruby, vous pouvez utiliser les méthodes `.day`, `.month`, `.year` pour extraire la journée, le mois et l'année des dates respectives. Ensuite, vous pouvez utiliser l'opérateur de comparaison `<=>` (spaceship) pour déterminer si une date est avant, après ou égale à une autre.

```
date_1 = Date.new(2020, 4, 1)
date_2 = Date.new(2020, 4, 5)

puts date_1 <=> date_2  # renvoie -1 (date_1 est avant date_2)
puts date_2 <=> date_1  # renvoie 1 (date_2 est après date_1)
puts date_1 <=> date_1  # renvoie 0 (date_1 est égale à date_1)
```

Vous pouvez également utiliser la méthode `.between?` pour vérifier si une date est comprise entre deux autres dates.

```
date_1 = Date.new(2020, 4, 1)
date_2 = Date.new(2020, 4, 5)
date_3 = Date.new(2020, 4, 3)

puts date_3.between?(date_1, date_2)  # renvoie true (date_3 est entre date_1 et date_2)
```

## Plongée en profondeur

Lorsque vous comparez des dates en Ruby, il est important de comprendre que les années bissextiles peuvent affecter les résultats. Par exemple, la méthode `.between?` vérifie si la date est comprise entre la première et la deuxième date, mais elle ne vérifie pas si la première date est avant ou après la deuxième. Cela signifie qu'une date comme le 29 février pourrait être considérée comme ne faisant pas partie de la même période si elle est comparée à des années bissextiles et non bissextiles. Pour éviter cela, vous pouvez utiliser la méthode `.compare_by ==` pour comparer les dates en utilisant une échelle équivalente.

```
date_1 = Date.new(2016, 2, 29)
date_2 = Date.new(2019, 2, 28)

puts date_1.between?(date_2, date_2 + 4.years)  # renvoie false (2019 n'est pas bissextile)
puts date_1.compare_by == date_2  # renvoie true (les deux dates sont considérées comme équivalentes)
```

## Voir aussi

- La documentation officielle de Ruby sur les dates et le temps: https://ruby-doc.org/stdlib-2.6.3/libdoc/date/rdoc/Date.html
- Un guide complet pour travailler avec les dates en Ruby: https://www.rubyguides.com/2015/12/ruby-time/
- Des exemples de code pour comparer les dates en Ruby: https://www.ruby-forum.com/t/comparing-date-values/121615/2