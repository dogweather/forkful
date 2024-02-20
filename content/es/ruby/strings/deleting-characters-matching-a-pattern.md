---
date: 2024-01-20 17:43:06.018261-07:00
description: "Eliminar caracteres que coinciden con un patr\xF3n es simplemente decirle\
  \ a tu programa que busque y quite ciertas secuencias o tipos de caracteres de una\u2026"
lastmod: 2024-02-19 22:05:18.081593
model: gpt-4-1106-preview
summary: "Eliminar caracteres que coinciden con un patr\xF3n es simplemente decirle\
  \ a tu programa que busque y quite ciertas secuencias o tipos de caracteres de una\u2026"
title: "Eliminando caracteres que coinciden con un patr\xF3n"
---

{{< edit_this_page >}}

## Qué y Por Qué?

Eliminar caracteres que coinciden con un patrón es simplemente decirle a tu programa que busque y quite ciertas secuencias o tipos de caracteres de una cadena de texto. Los programadores hacen esto para limpiar datos, preparar texto para procesamiento o eliminar información innecesaria.

## Cómo:

Ruby es genial para tratar con patrones, y aquí van unos ejemplos claros:

```Ruby
# Usando gsub con una expresión regular para eliminar todos los dígitos
cadena = "Yo tengo 2 manzanas y 3 plátanos."
limpia = cadena.gsub(/\d/, '')
puts limpia  # => "Yo tengo  manzanas y  plátanos."

# Eliminando solo ciertos caracteres específicos
cadena = "¡Bienvenidos a #Ruby 2023!"
limpia = cadena.delete("#")
puts limpia  # => "¡Bienvenidos a Ruby 2023!"

# Usando `tr` para borrar todos los caracteres excepto los alfabéticos
cadena = "precio: $59.99, código: #A31B!"
limpia = cadena.tr('^A-Za-z', '')
puts limpia  # => "precioxcdigoAB"
```

## Deep Dive

Eliminar caracteres no es nuevo; desde los días del ASCII en los años 60, los programas han necesitado "limpieza". En Ruby, `gsub` y `delete` son métodos directos y potentes de `String` que usan patrones o listas de caracteres a eliminar. Mientras `gsub` permite expresiones regulares, permitiendo patrones complejos, `delete` es más sencillo para listas concretas de caracteres. Hay alternativas como `tr`, que básicamente traduce caracteres, pero también se puede usar para eliminar cuando se define correctamente el conjunto de caracteres de destino.

## See Also

- Documentación de Ruby sobre los métodos `String#gsub`: [Ruby Docs gsub](https://ruby-doc.org/core-2.7.0/String.html#method-i-gsub)
- Regex en Ruby para patrones más avanzados: [Ruby Docs Regexp](https://ruby-doc.org/core-2.7.0/Regexp.html)
- Tutorial de Ruby sobre Expresiones Regulares: [Ruby Regexp Tutorial](https://www.rubyguides.com/2015/06/ruby-regex/)
