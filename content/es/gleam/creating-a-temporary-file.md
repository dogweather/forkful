---
title:                "Creando un archivo temporal"
html_title:           "Gleam: Creando un archivo temporal"
simple_title:         "Creando un archivo temporal"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/gleam/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## ¿Por qué crear un archivo temporal en Gleam?

La creación de un archivo temporal en Gleam es una técnica útil para manejar datos temporales en nuestra aplicación. Al crear un archivo temporal, podemos almacenar temporalmente información que no es necesaria a largo plazo, como datos de sesión, registros temporales o caché de información.

## Cómo crear un archivo temporal en Gleam

Para crear un archivo temporal en Gleam, podemos utilizar la función `Graphics.File.temp_file` que nos permite especificar una extensión para el archivo temporal y opcionalmente una ruta específica donde se almacenará. A continuación, utilizaremos la función `File.write` para escribir los datos que queremos almacenar en el archivo temporal.

```Gleam
import gleam/graphics/file

let { ok, path } =
  File.temp_file(ext: ".txt", dir: "/tmp/")

case ok {
  Ok -> File.write(path, "¡Hola Mundo!")
  Err(err) -> Err(err)
}
```

El código anterior crea un archivo temporal con una extensión ".txt" y lo almacena en la ruta "/tmp/". Luego, escribe el texto "¡Hola Mundo!" dentro del archivo. Si la creación del archivo y la escritura son exitosas, la función `temp_file` devuelve un `path` al archivo temporal.

También podemos utilizar el módulo `Random` para generar nombres aleatorios para nuestros archivos temporales si no queremos especificar una extensión o ruta.

```Gleam
import gleam/random
import gleam/graphics/file

let { ok, path } =
  File.temp_file(
    ext: Random.string(10),
    dir: Random.string(10)
  )

case ok {
  Ok -> File.write(path, "¡Hola Gleammers!")
  Err(err) -> Err(err)
}
```

## Profundizando en la creación de archivos temporales

La función `Graphics.File.temp_file` es una abstracción que utiliza las funciones `temp_dir` y `temp_name` del módulo `File`. Al utilizar estas funciones, podemos tener un mayor control sobre cómo se crea y se nombra nuestro archivo temporal. Además, podemos especificar si queremos que el archivo se elimine automáticamente después de un período de tiempo o si queremos borrarlo manualmente.

## Ver también

- [Documentación de Gleam sobre creación de archivos temporales](https://gleam.run/documentation/)
- [Gleam Random Module](https://gleam.run/documentation/stdlib/random/)