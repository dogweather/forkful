---
title:                "Capitalizando una cadena de texto"
date:                  2024-03-25T17:31:51.230740-06:00
model:                 gpt-4-0125-preview
changelog:
  - 2024-03-25, dogweather, edited and tested
  - 2024-03-25, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## ¿Qué y por qué?
Capitalizar una cadena generalmente significa convertir el primer carácter de una cadena a mayúscula y el resto a minúsculas. Pero a veces puede significar sólo asegurarse de que el primer carácter está en mayúscula mientras que el resto de la cadena permanece sin cambios. Honestamente, en mi opinión, es un término algo vago.

## Cómo hacerlo:
Ruby ofrece [métodos directos para la manipulación de cadenas](https://docs.ruby-lang.org/es/3.3/String.html), incluyendo la capitalización:

```ruby
# Método incorporado de Ruby
cadena = "hola MUNDO"
cadena_capitalizada = cadena.capitalize
puts cadena_capitalizada # => "Hola mundo"
```

Muy práctico.

El método `.capitalize` de Ruby es conveniente pero solo convierte a mayúscula la primera letra. Para tener más control o para capitalizar cada palabra en una cadena (conocido como caso de título), es posible que quieras usar el método `titleize` de la extensión ActiveSupport de Rails, o implementarlo tú mismo:

```ruby
# Usando 'titleize' de ActiveSupport en Rails
require 'active_support/core_ext/string/inflections'
cadena = "hola mundo"
puts cadena.titleize # => "Hola Mundo"
```

```ruby
# Una solución casera
cadena = "hola mundo"
capitalizada_cada_palabra = cadena.split.map(&:capitalize).join(' ')
puts capitalizada_cada_palabra # => "Hola Mundo"
```

Este método divide la cadena en un arreglo de palabras, capitaliza cada una, y luego las une de nuevo con un espacio.

Personalmente, llevo esta idea mucho más allá en mi código. Escribí mi propio [método `titleize` que tiene en cuenta palabras pequeñas como "a" y "the"](https://github.com/public-law/law_string/blob/master/lib/law_string.rb).
