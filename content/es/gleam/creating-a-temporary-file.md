---
title:    "Gleam: Creando un archivo temporal"
keywords: ["Gleam"]
---

{{< edit_this_page >}}

## ¿Por qué crear un archivo temporal en Gleam?

A veces, al escribir código en Gleam, es necesario crear un archivo temporal para almacenar información temporalmente. Ya sea para realizar una operación compleja o para manejar datos que no se guardarán permanentemente, crear un archivo temporal es una práctica común en la programación. A continuación, te mostraremos cómo crear un archivo temporal en Gleam.

## Cómo crear un archivo temporal

Utilizando la librería estándar de Gleam, `gleam/os`, podemos utilizar la función `io::file::tempdir()` para crear un directorio temporal en el sistema de archivos. Luego, podemos utilizar `TempDir::create()` para crear un archivo dentro del directorio temporal. Veamos un ejemplo:

```Gleam
import gleam/os
import gleam/tempdir

fn create_temp_file() {
  let temp_dir = io::file::tempdir("my-temp-dir") // Creamos un directorio temporal
  let temp_file = TempDir::create(temp_dir, "my-file.txt") // Creamos un archivo dentro del directorio temporal
  temp_file
}

create_temp_file() // Se devuelve una ruta al archivo creado
```

Este código creará un directorio temporal llamado "my-temp-dir" y un archivo dentro de él llamado "my-file.txt". Podemos utilizar la ruta devuelta por la función `create_temp_file()` para escribir y leer datos en este archivo como lo hacemos con cualquier otro archivo.

## Profundizando en la creación de archivos temporales

Cuando creamos un archivo temporal en Gleam, también podemos especificar el prefijo y sufijo del nombre del archivo utilizando la función `TempDir::create/3`. Además, podemos especificar que el archivo debe ser eliminado al finalizar el proceso utilizando la función `TempDir::create_cleanup/3`. Asimismo, si necesitamos un directorio temporal para almacenar varios archivos, podemos usar la función `io::file::tempdir_cleanup/2` para crear un directorio temporal que será eliminado junto con todos los archivos en él al finalizar el proceso.

## Ver también

- Documentación de la función `io::file::tempdir` en la documentación oficial de Gleam: https://gleam.run/documentation/stdlib/io#file-tempdir
- Ejemplos de uso de archivos temporales en Gleam: https://github.com/gleam-lang/stdlib/blob/master/io.coffee#L328-L353