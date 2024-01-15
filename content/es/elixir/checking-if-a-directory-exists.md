---
title:                "Comprobando si existe un directorio"
html_title:           "Elixir: Comprobando si existe un directorio"
simple_title:         "Comprobando si existe un directorio"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/elixir/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Por qué
La comprobación de la existencia de un directorio es una tarea común en la programación, especialmente cuando se trabaja con archivos y manipulación de datos. Al asegurarse de que un directorio existe antes de utilizarlo, podemos evitar errores y asegurarnos de que nuestro código funcione de manera fluida.

## Cómo hacerlo
En Elixir, podemos utilizar la función `File.dir?/1` para comprobar si un directorio existe en una ruta específica. Esta función devolverá un valor booleano, `true` si el directorio existe y `false` si no lo hace.

```Elixir
iex> File.dir?("/Users/usuario/Documentos/Proyecto")
true

iex> File.dir?("/Users/usuario/Documentos/NoExiste")
false
```

Además, podemos utilizar la función `File.cwd/0` para obtener el directorio actual en el que estamos trabajando y luego utilizarla junto con `File.join/2` para construir una ruta relativa al directorio actual.

```Elixir
iex> File.dir?(File.join(File.cwd, "ejemplo"))
true
```

Si necesitamos comprobar si un directorio específico existe dentro de un directorio, podemos utilizar la función `Path.expand/2` para construir la ruta completa y luego pasarla a `File.dir?/1`.

```Elixir
iex> File.dir?(Path.expand("/ejemplo/otro_directorio"))
true
```

## Profundizando
La función `File.dir?/1` en realidad llama a la función `:file.cwd?/1` de la librería Erlang, la cual permite interactuar con el sistema de archivos del sistema operativo.

Además, cuando utilizamos `File.dir?/1` en una ruta relativa, se utilizará el directorio actual del proceso Elixir en lugar del directorio actual del sistema operativo. Esto significa que si cambiamos el directorio actual utilizando `File.cd/1`, la ruta relativa también cambiará y puede afectar la comprobación de la existencia del directorio.

## Vea también
- Documentación oficial de Elixir para `File.dir?/1`: https://hexdocs.pm/elixir/File.html#dir?/1
- Documentación oficial de Erlang para `:file.cwd?/1`: http://erlang.org/doc/man/file.html
- Documentación oficial de Elixir para `File.cwd/0`: https://hexdocs.pm/elixir/File.html#cwd/0
- Documentación oficial de Elixir para `File.join/2`: https://hexdocs.pm/elixir/File.html#join/2
- Documentación oficial de Elixir para `Path.expand/2`: https://hexdocs.pm/elixir/Path.html#expand/2