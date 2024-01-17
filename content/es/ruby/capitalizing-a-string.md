---
title:                "Capitalizar una cadena"
html_title:           "Ruby: Capitalizar una cadena"
simple_title:         "Capitalizar una cadena"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/ruby/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Qué & Por qué?
Capitalizar una cadena significa convertir la primera letra de cada palabra en mayúscula. Los programadores hacen esto para mejorar la legibilidad y hacer que el texto se vea más organizado y profesional.

## Cómo:
Puedes utilizar el método `capitalize` en una cadena de texto para capitalizarla. Por ejemplo:

```
texto = "hola, soy una cadena de texto"
puts texto.capitalize
```
Output:
```
Hola, soy una cadena de texto
```

También puedes utilizar el método `titleize` para capitalizar cada palabra en una cadena de texto, incluso si no están separadas por espacios. Por ejemplo:

```
texto = "holaSoyUnaCadenaDeTexto"
puts texto.titleize
```
Output:
```
Hola Soy Una Cadena De Texto
```

## Profundizando:
La capitalización de cadenas es una convención de estilo que se ha utilizado desde los primeros días de la programación para mejorar la legibilidad del código. También existen otras técnicas para capitalizar cadenas, como el método `upcase` que convierte todas las letras en mayúscula o el método `downcase` que las convierte en minúscula.

## Ver también:
- [Documentación oficial de Ruby sobre strings](https://ruby-doc.org/core-2.7.1/String.html)
- [Método `capitalize` vs `titleize` en Ruby](https://www.rubyguides.com/2017/04/string-capitalization-ruby/)