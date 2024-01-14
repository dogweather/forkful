---
title:    "Gleam: Redacción en el error estándar"
keywords: ["Gleam"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/es/gleam/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Por qué escribir a la salida de error estándar en Gleam

Cuando estamos codificando en Gleam, es importante tener formas eficientes de manejar posibles errores o excepciones en nuestro código. Una forma de hacerlo es a través de la función `io.standard_error/1`. Esto nos permite escribir mensajes personalizados a la salida de error estándar, lo que puede ayudarnos a entender y solucionar problemas en nuestro código.

## Cómo hacerlo en Gleam
```Gleam
fn escribir_a_error(valor) {
  case valor {
    Ok -> "¡Todo bien!"
    Error(error) -> io.standard_error("¡Error encontrado: $(error)")
  }
}
```
En este ejemplo, usamos la función `io.standard_error/1` para escribir un mensaje personalizado si ocurre un error al ejecutar la función `escribir_a_error`. Al llamar a la función `escribir_a_error`, podemos ver que su resultado se envía a la salida de error estándar utilizando la sintaxis de la cadena de interpolación `$(...)`.

La salida de error estándar también se puede utilizar para mostrar información de depuración en tiempo de ejecución. Por ejemplo, si queremos ver los valores de algunas variables en un punto específico de nuestro código, podemos usar la función `io.println/1` dentro de un bloque `if` y así obtener esta información en la salida de error estándar.

```Gleam
let nombre = "Juan"
let ciudad = "Madrid"

if nombre == "Juan" {
  io.standard_error("El nombre es correcto: $(nombre)")
}

io.println("Mi ciudad es: $(ciudad)")
```

En este caso, solo el nombre "Juan" coincidirá con la condición del `if`, por lo que tendremos "El nombre es correcto: Juan" impreso en la salida de error estándar. Además, todas las veces que se llama a `io.println/1` se imprimirá un mensaje en la salida estándar. Con esto, podemos monitorear y depurar nuestro código de manera más efectiva.

## Profundizando en la escritura a la salida de error estándar
La función `io.standard_error/1` es parte del módulo `io`, que nos brinda funciones para leer y escribir en diferentes streams. Esta función en particular toma un argumento de tipo `String` y lo escribe en la salida de error estándar. También podemos pasar un segundo argumento opcional de tipo `Number` para especificar el código de error, lo que puede ser útil cuando necesitamos manejar errores específicos en nuestras aplicaciones.

Además de la función `io.standard_error/1`, también podemos utilizar `io.print/1` y `io.println/1` para escribir a la salida de error estándar, aunque estas son más adecuadas para la salida estándar. También podemos utilizar la macro `log!` del módulo `gleam.log`, que es una abstracción de la función `io.standard_error/2` y nos permite escribir mensajes de manera más fácil utilizando la sintaxis de la cadena de interpolación.

## Ver también
- Documentación oficial de Gleam sobre el módulo `io`: https://gleam.run/s/gbfhm9xj5w.pdf
- Ejemplos prácticos de uso de la salida de error estándar en Gleam: https://gleam.run/s/gwdvvqC3rO
- Entrada de blog de Gleam sobre el manejo de errores en Gleam: https://gleam.run/blog/error-handling-in-gleam.html