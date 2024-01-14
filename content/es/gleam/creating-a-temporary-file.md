---
title:                "Gleam: Creando un archivo temporal"
simple_title:         "Creando un archivo temporal"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/gleam/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Por qué

Crear un archivo temporario es una tarea común en la programación, ya que nos permite guardar datos de manera temporal durante la ejecución de un programa. Esto puede ser útil en situaciones en las que necesitamos almacenar información para luego utilizarla o compartir con otro programa.

## Cómo hacerlo

Para crear un archivo temporario en Gleam, podemos utilizar la función `File.create_temporary` y especificar la ruta y nombre del archivo que queremos crear. Por ejemplo:

```Gleam
let { ok, file } = File.create_temporary("./temp", "ejemplo.txt")
```

Podemos comprobar el resultado de la función usando el patrón de desestructuración, donde `ok` será `true` si el archivo fue creado exitosamente y `file` será el archivo temporario creado.

## En profundidad

Al crear un archivo temporario en Gleam, podemos especificar también el prefijo y sufijo del nombre del archivo. Esto nos permite distinguir fácilmente los archivos que hemos creado temporalmente de otros archivos.

Además, podemos utilizar la función `File.write` para escribir datos en el archivo temporario, y `File.read` para leer los datos almacenados en él. Al finalizar la ejecución del programa, el archivo temporario se eliminará automáticamente.

## Ver también

- Documentación sobre la función `File.create_temporary`: [https://gleam.run/core/file.html#create_temporary](https://gleam.run/core/file.html#create_temporary)
- Tutorial sobre el manejo de archivos en Gleam: [https://gleam.run/tutorials/files.html](https://gleam.run/tutorials/files.html)
- Ejemplos de uso de archivos temporarios en Gleam: [https://github.com/gleam-lang/gleam/blob/master/examples/file/create_temporary.gleam](https://github.com/gleam-lang/gleam/blob/master/examples/file/create_temporary.gleam)