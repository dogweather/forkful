---
title:                "Analyser une date depuis une chaîne de caractères"
date:                  2024-02-03T19:15:20.173020-07:00
model:                 gpt-4-0125-preview
simple_title:         "Analyser une date depuis une chaîne de caractères"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/ruby/parsing-a-date-from-a-string.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Quoi & Pourquoi ?
L'analyse d'une date à partir d'une chaîne de caractères consiste à convertir un texte représentant une date en un objet `Date` ou `DateTime` que Ruby comprend. Les programmeurs font cela pour effectuer des opérations telles que des comparaisons, des calculs ou des formats sur les dates, qui sont des tâches courantes dans les applications traitant de la planification, de l'analytique ou du traitement des données.

## Comment faire :
En Ruby, la bibliothèque standard fournit des moyens directs pour analyser les dates à partir de chaînes de caractères en utilisant les classes `Date` et `DateTime`. Voici comment vous le faites en utilisant les méthodes intégrées de Ruby :

```ruby
require 'date'

# Analyser une date à partir d'une chaîne de caractères
date_string = "2023-04-01"
parsed_date = Date.parse(date_string)
puts parsed_date
# => 2023-04-01

# DateTime pour une représentation du temps plus détaillée
datetime_string = "2023-04-01T15:30:45+00:00"
parsed_datetime = DateTime.parse(datetime_string)
puts parsed_datetime
# => 2023-04-01T15:30:45+00:00
```

Pour plus de contrôle ou pour gérer des formats que `parse` pourrait ne pas comprendre directement, vous pouvez utiliser `strptime` (analyse de chaîne de temps), en spécifiant explicitement le format :

```ruby
# Utilisation de strptime pour des formats personnalisés
custom_date_string = "01-04-2023"
parsed_date_custom = Date.strptime(custom_date_string, '%d-%m-%Y')
puts parsed_date_custom
# => 2023-04-01
```

### Utilisation de bibliothèques tierces :

Bien que les capacités intégrées de Ruby soient puissantes, parfois vous pourriez préférer les bibliothèques tierces pour des fonctionnalités supplémentaires ou une syntaxe plus simple. Un choix populaire est le gem `Chronic` pour l'analyse en langage naturel :

1. Tout d'abord, ajoutez Chronic à votre Gemfile et lancez `bundle install` :
```ruby
gem 'chronic'
```

2. Ensuite, utilisez-le comme ceci :
```ruby
require 'chronic'

parsed_chronic = Chronic.parse('next Tuesday')
puts parsed_chronic
# Le résultat variera selon la date actuelle; suppose une analyse le 01-04-2023
# => 2023-04-04 12:00:00 +0000
```

`Chronic` est très utile pour l'entrée utilisateur car il peut comprendre une large gamme de formats de date en langage naturel, le rendant ainsi un outil puissant pour les applications nécessitant une saisie de date flexible.
