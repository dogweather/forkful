---
title:                "Gleam: Impresión de resultados de depuración"
simple_title:         "Impresión de resultados de depuración"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/gleam/printing-debug-output.md"
---

{{< edit_this_page >}}

## Por qué imprimir la salida de depuración es importante en Gleam

Imprimir la salida de depuración es una técnica común utilizada por los programadores para identificar errores en su código. En Gleam, esta práctica puede ayudarnos a comprender mejor cómo funciona nuestro programa y a encontrar posibles problemas que necesiten ser resueltos.

## Cómo imprimir la salida de depuración en Gleam

Para imprimir la salida de depuración en Gleam, podemos utilizar la función `io.debug()` y pasarle como argumento el valor que deseamos imprimir. Por ejemplo:

```Gleam
fn main() {
    let num = 10
    io.debug("El valor de num es: ", num)
}
```

La salida de este código sería: `El valor de num es: 10`.

También podemos imprimir múltiples valores separándolos por comas dentro de la función `io.debug()`, como en el siguiente ejemplo:

```Gleam
fn main() {
    let name = "Juan"
    let age = 30
    io.debug("Hola, mi nombre es: ", name, "y tengo ", age, "años.")
}
```

La salida sería: `Hola, mi nombre es: Juan y tengo 30 años.` Este es solo un ejemplo simple, pero podemos utilizar la función `io.debug()` de diversas maneras dependiendo de nuestras necesidades de depuración.

## Profundizando en la impresión de salida de depuración en Gleam

Aunque imprimir la salida de depuración puede ser una herramienta útil, es importante no abusar de ella y asegurarse de eliminar todas las llamadas a la función `io.debug()` antes de publicar nuestro código en producción. También podemos utilizarla para imprimir la traza de una función, lo que nos permite seguir su ejecución y detectar posibles errores.

Otra forma interesante de utilizar `io.debug()` es imprimir valores de tipos de datos complejos, como registros o tuplas, lo que nos permite examinar su contenido y entender mejor cómo se están manipulando en nuestro programa.

## Ver también

- Documentación de la función `io.debug()` en la página oficial de Gleam: https://gleam.run/modules/gleam_io/#debug
- Ejemplos de depuración con Gleam: https://gleam.run/examples/debugging/
- Otros consejos para la depuración en Gleam: https://medium.com/the-gleam-programming-language/tips-for-debugging-in-gleam-6cd78b3c47b7