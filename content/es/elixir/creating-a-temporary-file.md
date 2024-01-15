---
title:                "Creación de un archivo temporal"
html_title:           "Elixir: Creación de un archivo temporal"
simple_title:         "Creación de un archivo temporal"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/elixir/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Por qué

Crear un archivo temporal es una tarea común en el desarrollo de aplicaciones. Puede ser útil para almacenar datos temporales, manipular archivos y realizar pruebas. Además, es una práctica común para mantener limpia la estructura de un proyecto eliminando archivos temporales después de su uso.

## Cómo hacerlo

Para crear un archivo temporal en Elixir, podemos usar la función `Tempfile.open/2` de la librería `:tempfile`. Esta función recibe dos argumentos: el nombre del archivo y una función anónima que será ejecutada con el nombre del archivo como parámetro.

``` Elixir
file_path = "/ruta/al/archivo/temporal.txt"
Tempfile.open("ejemplo", fn(file) ->
  IO.write(file, "Este es un archivo temporal")
  IO.puts("La ruta del archivo es: #{file_path}")
end)
```

Este código creará un archivo temporal llamado "ejemplo" y escribirá "Este es un archivo temporal" en él. Luego, imprimirá la ruta completa del archivo, que en este caso sería "/ruta/al/archivo/temporal.txt".

## Profundizando

La función `Tempfile.open/2` es solo una forma de crear archivos temporales en Elixir. También podemos usar la función `File.temp_file/1` para generar un archivo temporal con un nombre aleatorio en la carpeta temporal del sistema.

``` Elixir
file_path = File.temp_file()
IO.puts("El archivo temporal se encuentra en: #{file_path}")
```

Esta función crea un archivo temporal en la carpeta temporal del sistema, que puede variar según el sistema operativo. Podemos especificar una ruta opcional como argumento para la función `File.temp_file/1`, que creará el archivo en la ruta especificada en lugar de en la carpeta temporal del sistema.

En caso de que necesitemos una ruta más específica, podemos usar la función `File.temp_dir/1` para crear una carpeta temporal en la ruta especificada y luego usar la función `File.temp_file/2` para crear el archivo dentro de esa carpeta.

``` Elixir
dir_path = "/ruta/carpeta/temporal"
File.temp_dir(dir_path)

file_path = File.temp_file(dir_path)
IO.puts("El archivo temporal se encuentra en: #{file_path}")
```

Por último, para eliminar un archivo temporal, podemos usar la función `File.delete/1` especificando la ruta del archivo como argumento.

## Ver también

- [Documentación de Elixir acerca de archivos temporales](https://hexdocs.pm/elixir/File.html#temp_file/1)
- [Ejemplo de generación de archivos temporales en Elixir](https://gist.github.com/martinoamigo/d2ff139233a1b538a4ecab32615d3fc8)
- [Más información sobre manipulación de archivos en Elixir](https://elixir-lang.org/getting-started/io-and-the-file-system.html)