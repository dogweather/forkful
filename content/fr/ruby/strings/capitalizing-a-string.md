---
changelog:
- 2024-03-25, dogweather, edited and tested
- 2024-03-25, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:02:29.358527-07:00
description: "Capitaliser une cha\xEEne de caract\xE8res signifie g\xE9n\xE9ralement\
  \ convertir le premier caract\xE8re d'une cha\xEEne en majuscule et le reste en\
  \ minuscule. Mais\u2026"
lastmod: '2024-03-25T19:21:58.508306-06:00'
model: gpt-4-0125-preview
summary: "Capitaliser une cha\xEEne de caract\xE8res signifie g\xE9n\xE9ralement convertir\
  \ le premier caract\xE8re d'une cha\xEEne en majuscule et le reste en minuscule."
title: "Mettre en majuscule une cha\xEEne de caract\xE8res"
weight: 2
---

## Quoi & Pourquoi ?
Capitaliser une chaîne de caractères signifie généralement convertir le premier caractère d'une chaîne en majuscule et le reste en minuscule. Mais parfois, cela peut juste signifier s'assurer que le premier caractère est en majuscule tout en laissant le reste de la chaîne inchangé. Honnêtement, à mon avis, c'est un terme quelque peu vague.

## Comment faire :
Ruby propose [des méthodes directes pour la manipulation de chaînes de caractères](https://docs.ruby-lang.org/en/3.3/String.html), y compris pour la capitalisation :

```ruby
# Méthode intégrée de Ruby
string = "hello WORLD"
capitalized_string = string.capitalize
puts capitalized_string # => "Hello world"
```

Très pratique.

La méthode `.capitalize` de Ruby est pratique mais ne met en majuscule que la première lettre. Pour plus de contrôle ou pour capitaliser chaque mot d'une chaîne (connu sous le nom de casse de titre), vous pourriez vouloir utiliser la méthode `titleize` de l'extension Rails ActiveSupport, ou l'implémenter vous-même :

```ruby
# Utilisation de 'titleize' de ActiveSupport dans Rails
require 'active_support/core_ext/string/inflections'
string = "hello world"
puts string.titleize # => "Hello World"
```

```ruby
# Une solution maison
string = "hello world"
capitalized_each_word = string.split.map(&:capitalize).join(' ')
puts capitalized_each_word # => "Hello World"
```

Cette méthode divise la chaîne en un tableau de mots, met chaque mot en majuscule, puis les rejoint tous ensemble avec un espace.

Personnellement, je pousse cette idée beaucoup plus loin dans mon code. J'ai écrit ma propre [méthode `titleize` qui tient compte des petits mots comme "a" et "the"](https://github.com/public-law/law_string/blob/master/lib/law_string.rb).
