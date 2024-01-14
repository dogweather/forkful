---
title:    "Gleam: Escribir en el error estándar"
keywords: ["Gleam"]
---

{{< edit_this_page >}}

¡Hola a todos! ¿Alguna vez han encontrado un error en su programa de Gleam y no saben cómo solucionarlo? Bueno, en este blog post te enseñaré cómo escribir a la salida de error estándar en Gleam. ¡Comencemos!

## ¿Por qué?

Primero, es importante entender por qué escribir a la salida de error estándar podría ser útil. Cuando escribimos a esta salida, estamos indicando que algo ha ido mal en nuestro código y que necesitamos corregirlo. Esto nos ayuda a identificar y solucionar errores en nuestro programa de manera más eficiente.

## ¿Cómo hacerlo?

Es muy sencillo escribir a la salida de error estándar en Gleam. Simplemente usamos la función `stderr.write` seguida del mensaje que queremos imprimir. Veamos un ejemplo:

```Gleam
use gleam/io

fn print_error() {
  stderr.write("¡Oops, parece que hay un error!")
}

print_error()
```

El resultado sería el siguiente en la terminal:

```
¡Oops, parece que hay un error!
```

## Profundizando

Ahora que ya sabemos cómo escribir a la salida de error estándar, podemos profundizar un poco más en el tema. Podemos usar la función `stderr.write` para imprimir cualquier tipo de dato, ya sea una cadena de texto, un número o incluso una lista. Además, también podemos utilizar la función `stderr.writeln` para imprimir una cadena de texto seguida de un salto de línea. Veamos un ejemplo de ambos casos:

```Gleam
use gleam/io
use gleam/str

fn print_error() {
  // Imprime una cadena de texto
  stderr.write("¡Oops, parece que tenemos un error!")

  // Imprime un número
  stderr.write(str.to_int(42))

  // Imprime una lista
  stderr.write(str.to_list("Esto también es un error"))

  // Imprime una cadena de texto seguida de un salto de línea
  stderr.writeln("Todos cometemos errores, ¡pero también aprendemos de ellos!")
}

print_error()
```

El resultado sería el siguiente en la terminal:

```
¡Oops, parece que tenemos un error!
42
[69, 115, 116, 111, 32, 116, 97, 109, 98, 105, 233, 110, 32, 101, 115, 32, 117, 110, 32, 101, 114, 114, 111, 114]
Todos cometemos errores, ¡pero también aprendemos de ellos!
```

Con esto, ya sabes todo lo necesario para escribir a la salida de error estándar en Gleam. ¡Espero que este blog post te sea de ayuda!

## Ver también

- Documentación oficial de Gleam para la función `stderr.write`: [https://gleam.run/documentation/stdlib/io#write](https://gleam.run/documentation/stdlib/io#write)
- Documentación oficial de Gleam para la función `stderr.writeln`: [https://gleam.run/documentation/stdlib/io#writeln](https://gleam.run/documentation/stdlib/io#writeln)
- Artículo sobre manejo de errores en Gleam: [https://sheharyar.me/blog/error-handling-gleam/](https://sheharyar.me/blog/error-handling-gleam/)