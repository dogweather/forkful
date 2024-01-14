---
title:                "Ruby: Capitalizar una cadena"
programming_language: "Ruby"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/ruby/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## ¿Por qué capitalizar una cadena?

Capitalizar una cadena es una tarea común en la programación. Al capitalizar una cadena, se cambian todas las letras de la cadena a su forma mayúscula. Esto puede ser útil en situaciones como la validación de entradas del usuario o la manipulación de datos.

## Cómo hacerlo

Para capitalizar una cadena en Ruby, podemos utilizar el método `upcase` en la cadena. También podemos utilizar el método `capitalize` para capitalizar solo la primera letra de la cadena.

```Ruby
cadena = "hola mundo"
puts cadena.upcase 
# SALIDA: HOLA MUNDO

puts cadena.capitalize 
# SALIDA: Hola mundo
```

También podemos utilizar el método `upcase!` y `capitalize!` si queremos modificar la cadena original en lugar de crear una nueva.

```Ruby
cadena = "hola mundo"
cadena.upcase!
puts cadena
# SALIDA: HOLA MUNDO

cadena.capitalize!
puts cadena
# SALIDA: Hola mundo
```

También podemos capitalizar solo la primera letra de cada palabra en una cadena utilizando el método `titleize` del gem 'activesupport'.

```Ruby
require 'active_support/all' 
cadena = "hola mundo"
puts cadena.titleize
# SALIDA: Hola Mundo
```

## Profundizando

En Ruby, también podemos utilizar la función `scan` junto con una expresión regular para capitalizar solo ciertas letras en una cadena.

```Ruby
cadena = "código ruby 123"
regex = /(\w)(\w+)/
puts cadena.scan(regex) {|match| match[0].upcase + match[1]}
# SALIDA: Código Ruby 123
```

También podemos utilizar el método `tr` para capitalizar solo letras específicas en una cadena.

```Ruby
cadena = "cadena con letras minúsculas"
puts cadena.tr("a-z", "A-Z")
# SALIDA: CADENA CON LETRAS MINÚSCULAS
```

## Ver también

- [Ruby String documentation](https://ruby-doc.org/core-2.6/String.html)
- [Ruby regular expressions tutorial](https://www.rubyguides.com/2015/06/ruby-regex/)
- [ActiveSupport gem documentation](https://guides.rubyonrails.org/v3.2.13/active_support_core_extensions.html)