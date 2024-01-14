---
title:    "Gleam: Leyendo un archivo de texto."
keywords: ["Gleam"]
---

{{< edit_this_page >}}

## Por qué

Leer un archivo de texto puede parecer una tarea simple, pero es una habilidad fundamental que todo programador debe tener. Ya sea que estés trabajando en un proyecto personal o en un equipo, es importante saber cómo leer y manipular archivos de texto para acceder y utilizar la información que contienen. En este artículo, te mostraremos cómo hacerlo en Gleam.

## Cómo hacerlo

Para leer un archivo de texto en Gleam, primero necesitamos abrirlo con la función `File.open`, especificando el nombre del archivo y el modo de apertura (como `:read` para solo lectura). Luego, podemos utilizar la función `File.read_all` para leer el contenido del archivo de una sola vez y almacenarlo en una variable. Veamos un ejemplo en código:

```Gleam
let archivo = File.open("ejemplo.txt", :read)
let contenido = File.read_all(archivo)
```

En este caso, hemos abierto el archivo "ejemplo.txt" para lectura y leído todo su contenido, que quedará almacenado en la variable `contenido`. Ahora podemos utilizar esta información en nuestro código como deseemos.

## Profundizando

Pero, ¿qué sucede detrás de escena cuando leemos un archivo de texto en Gleam? La función `File.read_all` de hecho devuelve un valor de tipo `Result`, que puede ser o bien `Ok(contenido)` en caso de éxito o `Error(error)` si algo falla. Para manejar posibles errores, podemos hacer uso del bloque `case` para pattern matching y tratar cada caso por separado.

Además, existen otras funciones útiles para leer archivos de texto en Gleam, como `File.read_line` para leer una sola línea a la vez, o `File.read_with` para pasar una función que procese cada línea del archivo. Explora la documentación de Gleam para conocer más detalles y opciones.

## Ver también

- [Documentación de File en Gleam](https://gleam.run/modules/io/file.html)
- [Tutorial de archivo de texto en Gleam](https://gleam.run/articles/files.html)
- [Ejemplo de lectura de un archivo en Gleam](https://gist.github.com/gleam-lang/1ff8bd8c602cebc4fa8d3724727fa8ad)