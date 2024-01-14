---
title:                "Elixir: Comprobar si existe un directorio"
programming_language: "Elixir"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/elixir/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Por qué

La verificación de la existencia de un directorio es una tarea común en la programación en Elixir. Puede ser necesaria para garantizar que el directorio al que estamos apuntando existe antes de realizar una operación en él, o para evitar errores potenciales en nuestro código.

## Cómo hacerlo

Para verificar si un directorio existe en Elixir, podemos utilizar la función `File.dir?` pasando como argumento la ruta del directorio que queremos verificar. Esta función devolverá `true` si el directorio existe y `false` en caso contrario.

```Elixir
File.dir?("ruta/del/directorio")
# => true
```

También podemos utilizar `File.exists?` que funciona de la misma manera, pero también puede verificar la existencia de archivos.

```Elixir
File.exists?("ruta/del/archivo")
# => true
```

## Profundizando

Ambas funciones utilizan llamadas al sistema operativo para verificar la existencia de un directorio o archivo. En el caso de `File.dir?` se llama a la función `dir?` de la librería estándar de Erlang, mientras que `File.exists?` llama a la función `file:read_file_info` de Erlang.

Es importante tener en cuenta que estas funciones no hacen ninguna comprobación de permisos de acceso al directorio o archivo, por lo que puede devolver `true` incluso si no tenemos permisos para acceder al mismo.

## Ver También

- [Documentación de Elixir sobre la función File.dir?](https://hexdocs.pm/elixir/File.html#dir/1)
- [Documentación de Elixir sobre la función File.exists?](https://hexdocs.pm/elixir/File.html#exists/1)