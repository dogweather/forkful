---
title:                "Mettre une chaîne en majuscules"
date:                  2024-03-25T17:31:51.548013-06:00
model:                 gpt-4-0125-preview
changelog:
  - 2024-03-25, dogweather, edited and tested
  - 2024-03-25, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Quoi & Pourquoi ?
Mettre une chaîne de caractères en capitale signifie généralement convertir le premier caractère d'une chaîne en majuscule et le reste en minuscule. Mais parfois, cela peut simplement signifier s'assurer que le premier caractère est en majuscule tout en laissant le reste de la chaîne inchangé. Honnêtement, à mon avis, c'est un terme quelque peu vague.

## Comment faire :
Ruby propose [des méthodes directes pour la manipulation des chaînes de caractères](https://docs.ruby-lang.org/en/3.3/String.html), y compris pour la capitalisation :

```ruby
# Méthode intégrée de Ruby
string = "hello WORLD"
capitalized_string = string.capitalize
puts capitalized_string # => "Hello world"
```

Très pratique.

La méthode `.capitalize` de Ruby est pratique mais ne met en majuscule que la première lettre. Pour plus de contrôle ou pour mettre chaque mot d'une chaîne en majuscules (connu sous le nom de casse de titre), vous pourriez vouloir utiliser la méthode `titleize` de l'extension ActiveSupport de Rails, ou l'implémenter vous-même :

```ruby
# Utilisation de 'titleize' d'ActiveSupport dans Rails
require 'active_support/core_ext/string/inflections'
string = "hello world"
puts string.titleize # => "Hello World"
```

```ruby
# Une solution faite maison
string = "hello world"
capitalized_each_word = string.split.map(&:capitalize).join(' ')
puts capitalized_each_word # => "Hello World"
```

Cette méthode divise la chaîne en un tableau de mots, met chaque mot en majuscule, puis les réunit avec un espace.

Personnellement, je pousse cette idée beaucoup plus loin dans mon code. J'ai écrit ma propre méthode [`titleize` qui prend en compte les petits mots comme "a" et "the"](https://github.com/public-law/law_string/blob/master/lib/law_string.rb).
