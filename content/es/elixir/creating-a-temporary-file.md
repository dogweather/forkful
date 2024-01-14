---
title:                "Elixir: Creando un archivo temporal"
programming_language: "Elixir"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/elixir/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Por qué
Crear un archivo temporal es una práctica común en el desarrollo de software. Puede ser útil para almacenar datos temporales o para realizar pruebas sin afectar archivos existentes.

## Cómo
Para crear un archivo temporal en Elixir, utilizaremos la función `File.tempfile/1`. Esta función toma como argumento una lista de opciones y devuelve un objeto de archivo temporal.

```Elixir
# Ejemplo de código

{_, temp_file} = File.tempfile([suffix: ".txt"])
```

El primer elemento de la tupla devuelta por `File.tempfile/1` es el archivo de ruta donde se creará el archivo temporal. El segundo elemento es un objeto de archivo que se puede utilizar para escribir o leer datos del archivo temporal.

```Elixir
# Ejemplo de salida

iex> {_, temp_file} = File.tempfile([suffix: ".txt"])
{"/var/folders/03/tt64bgb93mn0p6t21ts5xhq00000gn/T/erl..Gh0r0", #PID<0.73.0>}

iex> IO.write(temp_file, "¡Hola, mundo!")
:ok

iex> File.read(temp_file)
"¡Hola, mundo!"
```

## Profundizar
La función `File.tempfile/1` tiene una opción adicional, `path`, que nos permite especificar la ubicación donde se creará el archivo temporal. Esto es útil si queremos tener control sobre dónde se crean los archivos temporales en nuestro sistema.

```Elixir
# Ejemplo de código

{_, temp_file} = File.tempfile([path: "../tmp", suffix: ".csv"])
```

También podemos utilizar la opción `prefix` para especificar un prefijo para el nombre del archivo. Esto puede ser útil para identificar fácilmente los archivos temporales en nuestro código.

```Elixir
# Ejemplo de código

{_, temp_file} = File.tempfile([prefix: "temp_", suffix: ".txt"])
```

## Ver también
- [Documentación oficial de Elixir para File.tempfile/1](https://hexdocs.pm/elixir/File.html#tempfile/1)
- [Artículo sobre creación de archivos temporales en Elixir](https://peterczar.github.io/2014/09/20/writing-files-in-memory-in-elixir/)
- [Página de blog sobre manipulación de archivos en Elixir](https://underjord.io/manipulating-files-and-folders-in-elixir.html)