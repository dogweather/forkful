---
changelog:
- 2024-03-25, dogweather, edited and tested
- 2024-03-25, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:02:29.358527-07:00
description: "Capitalizar una cadena generalmente significa convertir el primer car\xE1\
  cter de una cadena a may\xFAsculas y el resto a min\xFAsculas. Pero a veces puede\u2026"
lastmod: '2024-03-25T19:21:56.267612-06:00'
model: gpt-4-0125-preview
summary: "Capitalizar una cadena generalmente significa convertir el primer car\xE1\
  cter de una cadena a may\xFAsculas y el resto a min\xFAsculas. Pero a veces puede\u2026"
title: Capitalizando una cadena de texto
weight: 2
---

## ¿Qué y Por Qué?
Capitalizar una cadena generalmente significa convertir el primer carácter de una cadena a mayúsculas y el resto a minúsculas. Pero a veces puede significar simplemente asegurarse de que el primer carácter está en mayúscula mientras se deja el resto de la cadena sin cambios. Honestamente, en mi opinión, es un término algo vago.

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
