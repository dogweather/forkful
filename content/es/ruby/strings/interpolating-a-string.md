---
date: 2024-01-20 17:51:53.424787-07:00
description: "La interpolaci\xF3n de cadenas en Ruby permite incrustar el valor de\
  \ cualquier expresi\xF3n dentro de una cadena de texto. Los programadores lo hacen\
  \ para\u2026"
lastmod: '2024-03-13T22:44:59.576306-06:00'
model: gpt-4-1106-preview
summary: "La interpolaci\xF3n de cadenas en Ruby permite incrustar el valor de cualquier\
  \ expresi\xF3n dentro de una cadena de texto."
title: "Interpolaci\xF3n de cadenas de texto"
weight: 8
---

## Qué y Por Qué?

La interpolación de cadenas en Ruby permite incrustar el valor de cualquier expresión dentro de una cadena de texto. Los programadores lo hacen para construir cadenas dinámicamente, facilitando la inserción de variables y expresiones dentro de ellas sin romper el flujo.

## Cómo hacerlo:

Aquí tienes algunos ejemplos de cómo interpolamos cadenas en Ruby. Recuerda, todo lo que está dentro de `#{}` se evalúa y se convierte a una cadena.

```Ruby
nombre = "Mundo"
saludo = "Hola, #{nombre}!"
puts saludo  # => Hola, Mundo!

edad = 28
mensaje = "En 10 años tendrás #{edad + 10} años."
puts mensaje  # => En 10 años tendrás 38 años.

precio = 5.99
producto = "café"
informe = "El precio del #{producto} es $#{'%.2f' % precio}."
puts informe  # => El precio del café es $5.99.
```

## Conocimiento Detallado:

Interpolar una cadena es una función que Ruby maneja elegantemente. Surgió como una mejor alternativa a la concatenación, que era más verbosa y propensa a errores.

Alternativas:
- Concatenación: `saludo = 'Hola, ' + nombre + '!'`
- `sprintf` o el método `format`: `mensaje = sprintf('En %d años tendrás %d años.', 10, edad + 10)`
- La función `concatenate`: `puts 'Hola, ' << nombre << '!'`

Detalles de implementación: Ruby convierte lo que esté dentro de `#{}` a una cadena llamando al método `.to_s` sobre la expresión resultante. Si la cadena original está entre comillas simples, la interpolación no funcionará; debe estar entre comillas dobles o usar sintaxis de literales de cadena como `%Q{}`.

## Ver También:

- La documentación oficial de Ruby sobre [literales de cadena](https://docs.ruby-lang.org/en/2.6.0/syntax/literals_rdoc.html#label-Strings)
- Ejemplo de libro: ["The Well-Grounded Rubyist" de David A. Black](https://www.manning.com/books/the-well-grounded-rubyist), que aborda el tratamiento de cadenas en Ruby y prácticas recomendadas.
