---
title:    "Ruby: Comparer deux dates"
keywords: ["Ruby"]
---

{{< edit_this_page >}}

# Pourquoi

Les dates sont un élément essentiel dans la programmation. Il est courant de devoir comparer deux dates pour déterminer la chronologie ou la durée entre deux événements. Heureusement, Ruby offre des méthodes utiles pour effectuer cette tâche rapidement et efficacement.

# Comment Faire

Pour comparer deux dates en Ruby, nous pouvons utiliser la méthode `compare` de la classe `Date`. Cette méthode prend deux arguments et renvoie un entier qui indique la relation entre les deux dates. Voici un exemple de code :

```ruby
date1 = Date.parse("1 jan 2020")
date2 = Date.parse("15 jan 2020")

puts date1.compare(date2)
```

Cela renverra `0` si les deux dates sont égales, `-1` si la première date est antérieure à la deuxième et `1` si la première date est postérieure à la deuxième.

Nous pouvons également utiliser la méthode `between?` pour vérifier si une date est comprise entre deux autres dates. Voici un exemple :

```ruby
date1 = Date.parse("1 jan 2020")
date2 = Date.parse("15 jan 2020")
date3 = Date.parse("10 jan 2020")

puts date3.between?(date1, date2)
```

Cela renverra `true` car la date `date3` est comprise entre `date1` et `date2`.

# Plongée Profonde

En plongeant plus profondément dans la comparaison des dates en Ruby, nous pouvons également utiliser les méthodes `eql?` et `==` pour comparer deux dates. La méthode `eql?` compare non seulement les dates elles-mêmes, mais aussi leurs composants de temps. La méthode `==` ne compare que les dates et ignore les composants de temps.

Une autre astuce utile est d'utiliser les méthodes `strftime` et `strptime` pour formater et convertir les dates selon nos besoins. Par exemple, `date.strftime("%d/%m/%Y")` renverra une chaîne de caractères dans le format jour/mois/année pour la date donnée.

# Voir Aussi

Pour en savoir plus sur la manipulation des dates en Ruby, vous pouvez consulter les ressources suivantes :

- [Documentation officielle de Ruby sur les dates](https://ruby-doc.org/stdlib-2.7.0/libdoc/date/rdoc/Date.html)
- [Tutoriel en ligne sur les dates en Ruby](https://www.rubyguides.com/2015/05/ruby-time/)
- [Exemples de code pour comparer les dates en Ruby](https://www.ruby-forum.com/t/comparing-dates/61878)

Maintenant, vous avez les outils nécessaires pour comparer facilement deux dates en Ruby. Bonne programmation ! # Bonne programmation !