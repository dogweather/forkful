---
title:                "Gleam: Comprobando si existe un directorio"
programming_language: "Gleam"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/gleam/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Por qué
¿Te has preguntado alguna vez si un directorio existe en tu programa de Gleam? Puede parecer una pregunta trivial, pero en realidad es un problema común en la programación. Saber cómo verificar la existencia de un directorio es esencial para asegurarse de que tu código funcione correctamente y evitar errores.

## Cómo hacerlo
En Gleam, verificar la existencia de un directorio es muy sencillo. Solo necesitas utilizar la función `std.fs.dir_exists/1` y pasarle como argumento la ruta del directorio que quieres comprobar. Por ejemplo:

```Gleam
let existe = std.fs.dir_exists("/mi/directorio")
```

Esta función devolverá `true` si el directorio existe, o `false` si no existe.

Pero, ¿qué ocurre si quieres comprobar si un directorio existe dentro de otro directorio? En ese caso, puedes utilizar la función `std.fs.join/2` para unir las rutas de ambos directorios y pasarla como argumento a `std.fs.dir_exists/1`, como se muestra a continuación:

```Gleam
let existe = std.fs.dir_exists(std.fs.join("/mi/directorio", "subdirectorio"))
```

¿Qué pasa si el directorio que queremos comprobar está dentro de un subdirectorio de un subdirectorio? No hay problema, solo necesitas utilizar `std.fs.join/2` tantas veces como sea necesario para construir la ruta correcta.

## Profundizando
Si quieres saber más sobre cómo funciona la función `std.fs.dir_exists/1`, puedes consultar su documentación en [este enlace](https://gleam.run/std/fs.html#dir_exists). Allí encontrarás información sobre cómo maneja los errores y el formato de las rutas.

Además, si necesitas trabajar con directorios y archivos en general, te recomendamos echar un vistazo a las funciones de la librería estándar [`std.fs`](https://gleam.run/std/fs.html) y explorar todo lo que puedes hacer con ellas.

## Ver también
- [Documentación de `std.fs.dir_exists/1`](https://gleam.run/std/fs.html#dir_exists)
- [Funciones de la librería `std.fs`](https://gleam.run/std/fs.html)
- [Ejemplos de código de Gleam](https://github.com/gleam-lang/gleam/blob/master/examples)