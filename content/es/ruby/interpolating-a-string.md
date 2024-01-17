---
title:                "Interpolando una cadena"
html_title:           "Ruby: Interpolando una cadena"
simple_title:         "Interpolando una cadena"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/ruby/interpolating-a-string.md"
---

{{< edit_this_page >}}

## ¿Qué y por qué?
Interpolar una cadena de texto en Ruby significa insertar valores de variables dentro de una cadena existente. Los programadores lo hacen para ahorrar tiempo y hacer que su código sea más legible y fácil de entender.

## Cómo:
```Ruby
# Ejemplo de interpolación de cadena
nombre = "Juan"
puts "Hola #{nombre}, ¿cómo estás?" # salida: Hola Juan, ¿cómo estás?
```

## Profundizando:
La interpolación de cadenas en Ruby fue introducida en la versión 1.9 y ha sido un método ampliamente utilizado desde entonces. Alternativas a la interpolación incluyen el uso de la concatenación de cadenas o el formateo de cadenas mediante el método `sprintf`. La interpolación de cadenas se logra mediante el uso de la sintaxis `#{}` dentro de una cadena y se resuelve al ejecutar el código.

## Ver también:
- [Ruby Docs on interpolation](https://ruby-doc.org/core-3.0.0/doc/syntax/literals_rdoc.html#label-String+Interpolation)
- [Ruby Guides on string interpolation](https://www.rubyguides.com/2016/08/ruby-string-interpolation/)