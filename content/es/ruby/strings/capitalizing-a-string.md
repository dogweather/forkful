---
changelog:
- 2024-03-25, dogweather, edited and tested
- 2024-03-25, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:02:29.358527-07:00
description: "C\xF3mo hacerlo: Ruby proporciona [m\xE9todos directos para la manipulaci\xF3\
  n de cadenas](https://docs.ruby-lang.org/es/3.3/String.html), incluida la\u2026"
lastmod: '2024-03-25T19:21:56.267612-06:00'
model: gpt-4-0125-preview
summary: "Ruby proporciona [m\xE9todos directos para la manipulaci\xF3n de cadenas](https://docs.ruby-lang.org/es/3.3/String.html),\
  \ incluida la capitalizaci\xF3n."
title: Capitalizando una cadena de texto
weight: 2
---

## Cómo hacerlo:
Ruby proporciona [métodos directos para la manipulación de cadenas](https://docs.ruby-lang.org/es/3.3/String.html), incluida la capitalización:

```ruby
# Método incorporado de Ruby
string = "hello WORLD"
capitalized_string = string.capitalize
puts capitalized_string # => "Hello world"
```

Muy práctico.

El método `.capitalize` de Ruby es conveniente pero solo convierte en mayúscula la primera letra. Para tener más control o para capitalizar cada palabra en una cadena (conocido como caso de título), podrías querer usar el método `titleize` de la extensión ActiveSupport de Rails, o implementarlo tú mismo:

```ruby
# Usando 'titleize' de ActiveSupport en Rails
require 'active_support/core_ext/string/inflections'
string = "hello world"
puts string.titleize # => "Hello World"
```

```ruby
# Una solución casera
string = "hello world"
capitalized_each_word = string.split.map(&:capitalize).join(' ')
puts capitalized_each_word # => "Hello World"
```

Este método divide la cadena en un arreglo de palabras, capitaliza cada una, luego las une de nuevo con un espacio.

Personalmente, llevo esta idea mucho más lejos en mi código. Escribí mi propio [método `titleize` que tiene en cuenta palabras pequeñas como "a" y "the"](https://github.com/public-law/law_string/blob/master/lib/law_string.rb).
