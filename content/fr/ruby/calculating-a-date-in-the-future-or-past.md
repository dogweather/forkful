---
title:                "Ruby: Calculer une date dans le futur ou le passé"
simple_title:         "Calculer une date dans le futur ou le passé"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/ruby/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

# Pourquoi

Il y a de nombreuses raisons pour lesquelles quelqu'un voudrait calculer une date dans le futur ou dans le passé en utilisant Ruby. Cela peut être utile pour la planification d'événements, la gestion de tâches, ou simplement pour satisfaire sa curiosité.

# Comment faire

Calculer une date dans le futur ou dans le passé en Ruby est assez simple en utilisant la méthode `Date.new`. Voici un exemple de code pour calculer la date dans 7 jours à partir d'aujourd'hui :

```Ruby
Date.new(2020, 6, 25)+7
```

Cela nous donne la date du 2 juillet 2020. De même, pour calculer une date dans le passé, il suffit de soustraire le nombre de jours souhaité :

```Ruby
Date.new(2020, 6, 25)-7
```

Cela nous donne la date du 18 juin 2020.

Pour rendre le code encore plus dynamique, vous pouvez utiliser la méthode `.today` pour obtenir la date actuelle, et la méthode `.strftime` pour formater la date selon vos préférences. En voici un exemple :

```Ruby
today = Date.today    # obtient la date actuelle
future_date = today + 7    # ajoute 7 jours à la date actuelle
puts future_date.strftime("%d/%m/%Y")    # formate la date pour l'afficher au format jour/mois/année
```

Cela nous donne l'affichage suivant : `02/07/2020`.

# Plongez plus profondément

Maintenant que vous savez comment calculer une date dans le futur ou dans le passé en utilisant Ruby, voici quelques conseils supplémentaires :

- La méthode `Date.new` prend trois arguments obligatoires : l'année, le mois et le jour. Mais vous pouvez également utiliser des arguments optionnels comme l'heure, les minutes et les secondes pour créer des dates plus précises.

- Ruby a une bibliothèque standard étendue pour travailler avec les dates et les heures, y compris les méthodes pour comparer des dates, ajouter et soustraire des temps et des dates, et beaucoup plus encore. N'hésitez pas à explorer ces fonctions pour en savoir plus sur le travail avec les dates en Ruby.

- Si vous avez besoin de calculer des dates plus complexes, comme une date dans 2 mois à partir d'aujourd'hui, vous pouvez utiliser la gem `active_support` qui offre des méthodes supplémentaires pour travailler avec les dates.

# Voir aussi

- [Documentation officielle de Ruby sur les dates et les heures](https://ruby-doc.org/stdlib-2.7.1/libdoc/date/rdoc/Date.html)
- [Gem active_support pour des manipulations de dates plus avancées](https://rubygems.org/gems/active_support)
- [Un tutoriel sur la manipulation de dates en Ruby](https://www.rubyguides.com/2015/12/ruby-date-time/)
- [Exemples concrets d'utilisation de dates en Ruby](https://www.rubyguides.com/2016/06/ruby-date/)