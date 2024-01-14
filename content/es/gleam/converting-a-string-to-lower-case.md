---
title:    "Gleam: Convirtiendo una cadena a minúsculas"
keywords: ["Gleam"]
---

{{< edit_this_page >}}

## Por qué

En la programación, a menudo es necesario manipular cadenas de texto para llevar a cabo diversas tareas. Una de estas tareas puede ser convertir una cadena a minúsculas. Esta es una operación bastante común y útil, ya que permite estandarizar el formato de las cadenas de texto y facilita la comparación de estas. En esta entrada, aprenderemos cómo realizar esta conversión en el lenguaje de programación Gleam.

## Cómo hacerlo

Para convertir una cadena a minúsculas en Gleam, podemos utilizar la función `to_lower` del módulo `gleam/string`. Esta función toma como argumento una cadena de texto y devuelve la misma cadena con todos los caracteres en minúsculas.

```Gleam
import gleam/string.{to_lower}

let cadena = "Hola Mundo"
let resultado = to_lower(cadena)

io.format("El resultado es {}", [resultado])
```

El resultado de este código será `El resultado es hola mundo`. Como se puede ver, la cadena se ha convertido a minúsculas correctamente.

También podemos convertir una cadena a minúsculas al momento de definirla, utilizando la función `to_lower_case` directamente.

```Gleam
let cadena = string.to_lower_case("Hola Mundo")
io.format("La cadena en minúsculas es {}", [cadena])
```

Nuevamente, el resultado será `La cadena en minúsculas es hola mundo`.

## Profundizando

¿Cómo funciona realmente la conversión de una cadena a minúsculas en Gleam? La función `to_lower` llama a la función `erlang:lower/1`, que a su vez utiliza la librería `erlang_unicode` para realizar el cambio de mayúsculas a minúsculas. Esta librería incluye un conjunto de reglas para realizar esta operación de manera precisa y eficiente.

Además, es importante mencionar que esta función sólo convierte los caracteres alfabéticos en la cadena a minúsculas, no afectando a caracteres especiales u otros símbolos.

## Ver también

Si quieres aprender más sobre el uso de cadenas de texto en Gleam, puedes consultar la documentación oficial en el siguiente enlace: [https://gleam.run/book/std-lib-string.html](https://gleam.run/book/std-lib-string.html).

También puedes explorar otros módulos útiles para el manejo de cadenas, como `gleam/regex` para realizar búsquedas y reemplazos en cadenas, o `gleam/string/utf8` para trabajar con cadenas de texto en formato UTF-8.