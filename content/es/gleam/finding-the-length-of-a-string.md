---
title:                "Encontrando la longitud de una cadena"
html_title:           "Gleam: Encontrando la longitud de una cadena"
simple_title:         "Encontrando la longitud de una cadena"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/gleam/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## ¿Por qué?

¿Alguna vez has querido saber la longitud de una cadena de texto en Gleam? En este artículo, te enseñaremos cómo obtener la longitud de una cadena de forma sencilla y eficiente.

## Cómo
Para encontrar la longitud de una cadena en Gleam, puedes utilizar la función incorporada `String.length`. Esta función devuelve un número que representa la cantidad de caracteres en la cadena. Veamos un ejemplo:
 
```Gleam
let cadena = "Hola Gleam!"
let longitud = String.length(cadena)
```

Si imprimimos `longitud` en la consola, obtendremos el siguiente resultado:
 
```
12
```

## Deep Dive
Gleam es un lenguaje de programación estático y por lo tanto, ofrece una forma segura de trabajar con cadenas. Esto significa que no tenemos que preocuparnos por errores de tiempo de ejecución que podrían ocurrir en otros lenguajes. Por ejemplo, si intentamos acceder a un carácter en una posición que excede la longitud de la cadena, Gleam nos mostrará un error en tiempo de compilación en lugar de darnos un resultado inesperado.

Además, la función `String.length` también es útil para verificar si una cadena está vacía. Si la longitud de la cadena es cero, significa que la cadena está vacía. Esto puede ser útil para validar la entrada de los usuarios o para comprobar si un archivo de texto está en blanco antes de abrirlo.

## See Also
A continuación, te dejamos algunos enlaces que pueden ser útiles para seguir aprendiendo sobre Gleam y la manipulación de cadenas en este lenguaje:

- [La documentación oficial de Gleam sobre la función String.length](https://gleam.run/documentation/std/string#length)
- [Un artículo en inglés sobre cómo trabajar con cadenas en Gleam](https://www.viget.com/articles/manipulating-strings-in-gleam/)
- [El repositorio oficial de Gleam en GitHub para explorar el código fuente](https://github.com/gleam-lang/gleam)

¡Esperamos que este artículo te haya sido útil para entender cómo obtener la longitud de una cadena en Gleam! ¡Hasta la próxima!