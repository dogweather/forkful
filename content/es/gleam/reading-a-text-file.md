---
title:                "Leyendo un archivo de texto"
html_title:           "Gleam: Leyendo un archivo de texto"
simple_title:         "Leyendo un archivo de texto"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/gleam/reading-a-text-file.md"
---

{{< edit_this_page >}}

## ¿Qué y Por Qué?
Leer un archivo de texto es simplemente leer el contenido de un archivo que contiene texto. Los programadores lo hacen para poder trabajar con la información del archivo y realizar diferentes operaciones en él.

## Cómo Hacerlo:
Para leer un archivo de texto en Gleam, primero necesitas abrir el archivo usando la función `std.fs.read_file`. Luego, puedes usar la función `file::read` para leer el contenido del archivo en una cadena de texto. Aquí hay un ejemplo:

```
archivo := std.fs.read_file("ruta/del/archivo")
texto := file::read(archivo)
```

Si el archivo no se puede abrir o leer correctamente, la función devolverá un error. En caso de que el archivo se lea exitosamente, `texto` contendrá el contenido del archivo en forma de cadena de texto.

## Profundizando:
Leer archivos de texto ha sido una tarea común para los programadores desde los primeros días de la programación. En Gleam, el enfoque en la seguridad de tipos y la concurrencia hace que sea seguro y fácil trabajar con archivos de texto. Alternativas al método descrito anteriormente incluyen el uso de bibliotecas de terceros como `std.text` o `std.io` para leer archivos de manera más eficiente.

## Vea También:
- Documentación oficial de Gleam sobre lectura de archivos: https://gleam.run/book/tour/files
- Ejemplo de lectura de un archivo de texto en Gleam: https://github.com/gleam-lang/example-project/blob/master/examples/file_io/file_io.gleam