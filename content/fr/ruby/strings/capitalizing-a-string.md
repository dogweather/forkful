---
changelog:
- 2024-03-25, dogweather, edited and tested
- 2024-03-25, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:02:29.358527-07:00
description: "Comment faire : Ruby propose [des m\xE9thodes directes pour la manipulation\
  \ de cha\xEEnes de caract\xE8res](https://docs.ruby-lang.org/en/3.3/String.html),\
  \ y\u2026"
lastmod: '2024-04-05T21:53:59.805543-06:00'
model: gpt-4-0125-preview
summary: "Ruby propose [des m\xE9thodes directes pour la manipulation de cha\xEEnes\
  \ de caract\xE8res](https://docs.ruby-lang.org/en/3.3/String.html), y compris pour\
  \ la capitalisation ."
title: "Mettre en majuscule une cha\xEEne de caract\xE8res"
weight: 2
---

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
