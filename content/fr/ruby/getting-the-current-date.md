---
title:    "Ruby: Obtenir la date actuelle"
keywords: ["Ruby"]
---

{{< edit_this_page >}}

## Pourquoi

Savez-vous quelle est la date d'aujourd'hui ? Que vous soyez en train de créer une application, de gérer un calendrier ou simplement de vouloir afficher la date sur votre site web, il est important de toujours avoir la date la plus récente. Cela peut sembler une tâche simple, mais il y a plusieurs raisons pour lesquelles vous voudriez intégrer la date courante dans votre code Ruby.

## Comment faire

Heureusement, Ruby a une méthode intégrée pour obtenir la date actuelle. Utilisez simplement la méthode `Time.now` pour récupérer la date et l'heure actuelles. Vous pouvez également utiliser les méthodes `year`, `month` et `day` pour obtenir les parties spécifiques de la date.

Voici un exemple de code Ruby pour récupérer la date et l'heure actuelles :

```Ruby
time = Time.now
puts time # affiche la date et l'heure actuelles

year = time.year
month = time.month
day = time.day
puts "Nous sommes le #{day}/#{month}/#{year}" # affiche la date actuelle au format jj/mm/aaaa
```

Si vous souhaitez ajouter un peu de mise en forme à votre date, vous pouvez utiliser la méthode `strftime`. Cette méthode vous permet de spécifier le format dans lequel vous souhaitez que la date soit affichée. Voici un exemple :

```Ruby
date = Time.now
puts date.strftime("%d %B, %Y") # affiche la date au format jour mois, année (par exemple "12 avril, 2019")
```

## Plongeon en profondeur

Maintenant que vous savez comment obtenir et afficher la date actuelle, vous vous demandez peut-être comment cela fonctionne réellement. En réalité, la méthode `Time.now` utilise la bibliothèque système du système d'exploitation pour obtenir la date et l'heure actuelles. Cela signifie que la date sera toujours exacte, quel que soit le fuseau horaire dans lequel vous vous trouvez.

Ruby a également d'autres méthodes pour travailler avec les dates, telles que `Date.parse` pour convertir une chaîne de caractères en objet date et `Date.today` pour obtenir la date actuelle sans heure.

## Voir aussi

- Documentation officielle de Ruby sur la classe `Time` : https://ruby-doc.org/core-2.6.3/Time.html
- Tutoriel sur les dates en Ruby : https://www.tutorialspoint.com/ruby/ruby_date_time.htm
- Formater des dates avec la méthode `strftime` : https://www.rubyguides.com/2015/06/ruby-date-format/
- Utiliser des dates dans Ruby on Rails : https://www.tutorialspoint.com/ruby-on-rails/rails-dates.htm