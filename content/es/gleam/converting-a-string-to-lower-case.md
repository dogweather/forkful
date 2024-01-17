---
title:                "Convirtiendo una cadena a minúsculas"
html_title:           "Gleam: Convirtiendo una cadena a minúsculas"
simple_title:         "Convirtiendo una cadena a minúsculas"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/gleam/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## ¡Qué & Por qué?
Convertir una cadena de texto a minúsculas es una tarea común en la programación en la que se cambia una cadena de texto a letras minúsculas en lugar de mayúsculas. Los programadores a menudo lo hacen para facilitar la comparación de cadenas de texto o para mejorar la legibilidad del código.

## Cómo:
Hay varias formas de convertir una cadena de texto a minúsculas en Gleam. Una opción es utilizar la función `String.to_lower` que toma una cadena de texto como entrada y devuelve una nueva cadena en minúsculas. Por ejemplo:

```
Gleam> String.to_lower("HOLA")
"hola"
```

También se puede utilizar el operador `~=~` para hacer coincidir patrones en una expresión `case`. Por ejemplo:

```
Gleam> case "HOLA" {
         x = String.to_lower(x) -> x
       }
"hola"
```

## Profundizando:
Convertir cadenas de texto a minúsculas puede ser una práctica común en la programación, pero en realidad tiene sus raíces en la informática antigua. Originalmente, las computadoras solo podían manejar caracteres en mayúsculas mayores, por lo que la conversión a minúsculas era una forma de ampliar la capacidad de las máquinas para procesar texto.

Mientras que en Gleam, la función `String.to_lower` es la forma recomendada de convertir cadenas de texto a minúsculas, también se puede lograr mediante el uso de bibliotecas externas o escribiendo su propio código para iterar y cambiar cada carácter manualmente.

## Ver también:
- [Documentación oficial de Gleam sobre la función `String.to_lower`](https://gleam.run/documentation/stdlib/string/#to-lower)
- [Preguntas y respuestas relacionadas en Stack Overflow](https://stackoverflow.com/questions/tagged/gleam+string+lowercase)