---
title:                "Imprimiendo salida de depuración"
html_title:           "Arduino: Imprimiendo salida de depuración"
simple_title:         "Imprimiendo salida de depuración"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/gleam/printing-debug-output.md"
---

{{< edit_this_page >}}

# Debuggeando con Gleam: Una Guía a Imprimir Resultados de Debug

## ¿Qué es y Porqué?

Imprimir resultados de debug es una práctica que los programadores usan para inspeccionar los datos internos de un programa mientras se está ejecutando. Hacer esto ayuda a entender cómo es que el programa está funcionando y a detectar cualquier problema.

## ¿Cómo hacerlo?

Usamos la función`io.debug` para imprimir en la consola. Aquí hay un ejemplo:

```gleam
import gleam/io

fn main() {
  let datos = "Hello, Gleam!"
  io.debug(datos)
}
```

Esto va a imprimir:

```stdout
Hello, Gleam!
```

## Un vistazo más profundo

La impresión de debug tiene sus raíces en la era de programación más temprana cuando los errores tenían que ser identificados manualmente leyendo listados de código. Ahora, es una de las maneras más fáciles y directas de entender que es lo que está pasando dentro de tu código.

Sin embargo, hay alternativas a imprimir datos de debug. Podrías usar un depurador que te permite pausar tu programa y examinar el estado de tu aplicación, línea por línea. También, hay herramientas de logging que proveen una manera de colectar, almacenar, y analizar resultados de debug.

En cuanto a `io.debug`, esta implementación es bastante simple. Vale la pena mencionar que `io.debug` siempre devuelve `Ok(Nil)`, lo que significa que es fácil de usar en cualquier parte de tu código sin cambiar el resultado.

## Ver también

Si quieres aprender más, te recomendamos seguir estos enlaces:

- La [documentación oficial de Gleam](https://gleam.run/book/tour/file_io.html) te hará entender mejor la función `io.debug`.
- El [artículo en wikipedia sobre debug](https://es.wikipedia.org/wiki/Depuraci%C3%B3n) te dará un resumen de los métodos de debug.
- Para entender más sobre las alternativas a la impresión de debug, puedes leer la [documentación de The Elixir School](https://elixirschool.com/es/lessons/specifics/debugging) sobre debug en Elixir, que también aplica a Gleam.