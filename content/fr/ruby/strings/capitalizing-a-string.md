---
title:                "Mettre en majuscule une chaîne"
aliases:
- fr/ruby/capitalizing-a-string.md
date:                  2024-02-03T19:06:08.630539-07:00
model:                 gpt-4-0125-preview
simple_title:         "Mettre en majuscule une chaîne"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/ruby/capitalizing-a-string.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Quoi et pourquoi ?
Capitaliser une chaîne de caractères en programmation fait souvent référence à la conversion du premier caractère d'une chaîne en majuscule et le reste en minuscule. Les programmeurs font cela pour des raisons telles que se conformer aux conventions de nommage, rendre les sorties plus lisibles ou assurer la cohérence des données pour les comparaisons et le stockage.

## Comment faire :
Ruby fournit des méthodes simples pour la manipulation des chaînes de caractères, y compris la capitalisation. Voici comment vous pouvez capitaliser une chaîne de caractères en Ruby :

```ruby
# Méthode intégrée de Ruby
string = "hello world"
capitalized_string = string.capitalize
puts capitalized_string # => "Hello world"
```

La méthode `.capitalize` de Ruby est pratique mais n'affecte que la première lettre. Pour plus de contrôle ou pour capitaliser chaque mot dans une chaîne (connu sous le nom de casse de titre), vous pourriez vouloir utiliser la méthode `titleize` de l'extension Rails ActiveSupport, ou l'implémenter vous-même :

```ruby
# Utilisation de 'titleize' d'ActiveSupport dans Rails
require 'active_support/core_ext/string/inflections'
string = "hello world"
puts string.titleize # => "Hello World"
```

Si vous n'utilisez pas Rails ou préférez une solution purement Ruby, voici comment vous pourriez capitaliser chaque mot dans une chaîne de caractères :

```ruby
string = "hello world"
capitalized_each_word = string.split.map(&:capitalize).join(' ')
puts capitalized_each_word # => "Hello World"
```

Cette méthode divise la chaîne en un tableau de mots, met en majuscule chacun d'eux, puis les joint à nouveau ensemble avec un espace.
