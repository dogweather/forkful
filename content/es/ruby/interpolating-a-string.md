---
title:                "Interpolando una cadena de texto"
html_title:           "Haskell: Interpolando una cadena de texto"
simple_title:         "Interpolando una cadena de texto"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/ruby/interpolating-a-string.md"
---

{{< edit_this_page >}}

## ¿Qué & Por qué?

La interpolación de cadenas en Ruby es un proceso por el cual se incrustan variables o expresiones dentro de una cadena. Los programadores lo hacen para insertar dinámicamente valores dentro de una cadena, lo que puede mejorar la legibilidad y eficiencia del código.

## Cómo se hace:

Aquí es cómo puedes hacer la interpolación de cadenas en Ruby:

```Ruby
nombre = "Juan"
puts "Hola, #{nombre}!"
```

La salida sería:

```Ruby
"Hola, Juan!"
```

También puedes insertar expresiones:

```Ruby
edad = 15
puts "En cinco años, tendrás #{edad + 5} años."
```

La salida sería:

```Ruby
"En cinco años, tendrás 20 años."
```

## Profundizando:

Historicamente, la interpolación de cadenas en Ruby ha sido un componente clave desde sus primeras versiones. Proporciona una forma elegante y limpia para insertar variables y expresiones en cadenas.

En cuanto a alternativas, también puedes usar la concatenación de cadenas. Pero, esto puede resultar en código más verboso y menos legible:

```Ruby
nombre = "Juan"
puts "Hola, " + nombre + "!"
```
La salida sería la misma que antes. Sin embargo, la interpolación de cadenas es, generalmente, la opción preferida debido a su simplicidad y legibilidad.

En términos de cómo funciona internamente, cuando se encuentra con #{}, Ruby evalúa lo que está entre las llaves como Ruby puro y luego convierte el resultado en una cadena.

## También podrías ver:

Para más información sobre la interpolación de cadenas en Ruby, consulta las siguientes fuentes:

- [Interpolación de cadena en el libro de Ruby](http://rubylearning.com/satishtalim/ruby_string_interpolation.html)
- [Documento oficial de Ruby](https://ruby-doc.org/core-2.7.0/String.html#method-i-25-3C)