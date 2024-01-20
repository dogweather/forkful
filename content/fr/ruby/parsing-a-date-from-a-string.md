---
title:                "Analyser une date à partir d'une chaîne"
html_title:           "Clojure: Analyser une date à partir d'une chaîne"
simple_title:         "Analyser une date à partir d'une chaîne"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/ruby/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## Quoi & Pourquoi?

Dans la programmation, l'analyse d'une date depuis une chaîne signifie transformer une date écrite en texte en un format informatique qui peut être traité. Nous le faisons pour manipuler, calculer ou formater les dates dans nos applications.

## Comment faire:

Voici un exemple simple en Ruby:

```ruby
require 'date'

date = "23-11-2020"
parsed_date = Date.strptime(date, '%d-%m-%Y')

puts parsed_date
```

Execution:

```ruby
# => 2020-11-23
```

Ici, nous utilisons `strptime` de la classe `Date` pour analyser la date depuis une chaîne.

## Exploration en profondeur

Historiquement, le besoin d'analyser les dates n'est pas nouveau. Au début de l'informatique, les dates étaient souvent conservées en texte. Lorsque la gestion des dates est devenue trop complexe, les langages de programmation ont commencé à introduire des outils d'analyse.

En Ruby, en plus de `strptime`, nous avons la methode parse qui est un peu plus flexible:

```ruby
date = "2020-11-23"
parsed_date = Date.parse(date)

puts parsed_date
```

Résultat:

```ruby
# => 2020-11-23
```

La différence clé est que `strptime` a besoin d'un format précis, tandis que `parse` peut gérer différents formats.

Notez cependant que `parse` peut donner des résultats inattendus, en faisant des suppositions sur le format. Ainsi, il est souvent préférable de l'utiliser lorsque vous êtes sûr du format entrant.

## Voir aussi

Pour plus d'informations sur la gestion des dates en Ruby, consultez ces ressources :

- La documentation officielle de la classe Date : https://ruby-doc.org/stdlib-2.5.1/libdoc/date/rdoc/Date.html
- Un guide complet sur les dates et heures en Ruby : https://www.rubyguides.com/ruby-tutorial/ruby-date-format/
- Une discussion approfondie sur `strptime` vs `parse` : https://stackoverflow.com/questions/2630639/ruby-parse-date.