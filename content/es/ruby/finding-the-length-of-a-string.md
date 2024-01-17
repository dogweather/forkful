---
title:                "Encontrar la longitud de una cadena"
html_title:           "Ruby: Encontrar la longitud de una cadena"
simple_title:         "Encontrar la longitud de una cadena"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/ruby/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## ¿Qué y Por qué?
En programación, encontrar la longitud de una cadena significa determinar la cantidad de caracteres que contiene esa cadena. Los programadores a menudo necesitan encontrar la longitud de una cadena para realizar ciertas operaciones o cálculos en su código.

## Cómo:
```ruby
# Ejemplo 1
puts "Hola Mundo".length
# Output: 10

# Ejemplo 2
my_string = "¡Hola!"
puts my_string.length
# Output: 6
```

## Profundizando:
Algunos lenguajes de programación tienen funciones incorporadas para encontrar la longitud de una cadena, como `len ()` en Python. En Ruby, podemos utilizar el método `.length` en una cadena para encontrar su longitud. También podemos utilizar el método `.size` que funciona de la misma manera. Tenga en cuenta que los espacios en blanco y los caracteres especiales también se cuentan en la longitud de la cadena.

## Ver también:
- Ruby String Documentation (https://ruby-doc.org/core-3.0.1/String.html)
- Otros lenguajes de Programación también tienen formas de encontrar la longitud de una cadena, como `length ()` en Java.