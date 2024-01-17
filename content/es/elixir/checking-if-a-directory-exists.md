---
title:                "Verificando si existe un directorio."
html_title:           "Elixir: Verificando si existe un directorio."
simple_title:         "Verificando si existe un directorio."
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/elixir/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## ¿Qué y por qué?
Comprobar si un directorio existe es una acción común en la programación. Permite a los programadores verificar si un directorio determinado está presente en una ubicación específica en el sistema de archivos. Esto es importante para asegurar que un programa pueda acceder y utilizar los archivos adecuados.

## ¿Cómo hacerlo?
En Elixir, se puede verificar si un directorio existe utilizando la función ```File.dir?/2``` y pasando como parámetro la ruta al directorio. Aquí hay un ejemplo de cómo verificar si el directorio "documents" existe en la carpeta actual:
```
Elixir
iex> File.dir?("documents")
true
```
Si el directorio no existe, la función devolverá ```false```.

## Profundizando
Antes de Elixir 1.7, se utilizaba la función ```File.exists?/1``` para verificar la existencia de un directorio. Sin embargo, esta función ha sido eliminada en versiones más recientes en favor de ```File.dir?/2```.

En caso de que se quiera comprobar si un archivo específico existe en lugar de un directorio, se puede utilizar la función ```File.exist?/1``` en su lugar.

## Ver también
- Documentación oficial de Elixir sobre la función ```File.dir?/2``` (https://hexdocs.pm/elixir/File.html#dir?/2)
- Artículo sobre cómo trabajar con archivos y directorios en Elixir (https://elixir-lang.org/getting-started/file-operations.html)
- Otra forma de verificar la existencia de un directorio en Elixir utilizando la biblioteca standard ```Path.join/2``` (https://elixirforum.com/t/directory-exists-test-around-path-join/13363)