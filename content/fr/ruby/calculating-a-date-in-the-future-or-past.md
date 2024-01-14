---
title:    "Ruby: Calculer une date dans le futur ou le passé"
keywords: ["Ruby"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fr/ruby/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Pourquoi

Souvent, lors de la programmation en Ruby, nous avons besoin de calculer une date à l'avenir ou dans le passé. Cela peut être utile pour des tâches telles que la planification de rappels, les réservations ou d'autres fonctions qui impliquent des délais.

## Comment Faire

Heureusement, Ruby a une solide bibliothèque de gestion des dates et des heures qui facilite ce genre de calculs. Voici quelques exemples pour vous aider à comprendre comment procéder.

Pour calculer une date dans le futur, nous pouvons utiliser la méthode `+` avec un nombre spécifiant les jours à ajouter à la date actuelle :

```Ruby
# Déclaration d'une date d'aujourd'hui
aujourdhui = Date.today

# Ajout de 5 jours à la date actuelle
futur = aujourdhui + 5

# Affichage du résultat dans le terminal
puts "La date dans 5 jours sera : #{futur}"
```

Dans cet exemple, nous avons utilisé la classe `Date` pour créer une variable représentant la date d'aujourd'hui, puis nous avons ajouté 5 jours à l'aide de la méthode `+`. Enfin, nous avons affiché le résultat dans le terminal en utilisant la méthode `puts`.

De même, pour calculer une date dans le passé, nous pouvons utiliser la méthode `-` avec un nombre négatif représentant les jours à soustraire de la date actuelle :

```Ruby
# Déclaration d'une date d'aujourd'hui
aujourdhui = Date.today

# Soustraction de 10 jours à la date actuelle
passe = aujourdhui - 10

# Affichage du résultat dans le terminal
puts "La date il y a 10 jours était : #{passe}"
```

En utilisant ces méthodes, nous pouvons également calculer des dates dans le futur ou le passé en utilisant d'autres unités de temps telles que les semaines, les mois ou même les années.

## Plongée Profonde

Pour ceux qui souhaitent approfondir leurs connaissances sur la gestion des dates et des heures en Ruby, il existe plusieurs méthodes utiles pour effectuer des calculs plus précis.

Par exemple, la méthode `DateTime#>>` peut être utilisée pour ajouter un nombre spécifique de mois à une date donnée, en prenant en compte les années bissextiles :

```Ruby
# Déclaration d'une date d'aujourd'hui
aujourdhui = DateTime.now

# Ajout de 3 mois à la date actuelle
futur = aujourdhui >> 3

# Affichage du résultat dans le terminal
puts "Dans 3 mois, nous serons en : #{futur.strftime("%m/%Y")}"
```

Nous avons également la possibilité de définir des dates spécifiques en utilisant la méthode `Date#parse` qui prend une chaîne de caractères représentant une date en entrée et renvoie un objet `Date` :

```Ruby
# Déclaration d'une date spécifique
date = Date.parse("2022-01-01")

# Affichage du résultat dans le terminal
puts "La première journée de 2022 sera un #{Date::DAYNAMES[date.wday]}."
```

Il existe de nombreuses autres méthodes et fonctionnalités pour manipuler les dates en Ruby, alors n'hésitez pas à explorer la documentation pour en apprendre davantage.

## Voir Aussi

- [Documentation officielle de la bibliothèque de gestion des dates en Ruby](https://ruby-doc.org/stdlib-2.7.1/libdoc/date/rdoc/Date.html)
- [Tutoriel sur la manipulation des dates en Ruby](https://www.rubyguides.com/2015/03/ruby-date-time/)
- [Guide complet sur l'utilisation de la classe `Date` en Ruby](https://www.tutorialspoint.com/ruby/ruby_date_time.htm)