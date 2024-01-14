---
title:    "Elixir: Leyendo un archivo de texto"
keywords: ["Elixir"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/es/elixir/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Por qué leer un archivo de texto en Elixir

Leer un archivo de texto es una tarea común en la programación y puede ser especialmente útil en Elixir para procesar grandes cantidades de datos. Además, esta habilidad es esencial para manipular archivos de configuración o archivos de registro en una aplicación.

## Cómo leer un archivo de texto en Elixir

La forma más sencilla de leer un archivo de texto en Elixir es utilizando la función `File.read!/1` que devuelve el contenido del archivo en una cadena de caracteres. Por ejemplo, si queremos leer un archivo llamado `datos.txt` y guardar su contenido en una variable, el código sería:

```elixir
contenido = File.read!("datos.txt")
```

También podemos utilizar la función `File.stream!/1` para leer un archivo línea por línea. Esto puede ser útil si el archivo es muy grande y no queremos cargar todo su contenido en memoria. El código sería similar al siguiente:

```elixir
File.stream!("datos.txt")
|> Enum.each(fn linea -> IO.puts(linea) end)
```

## Profundizando en la lectura de archivos de texto en Elixir

Existen otras funciones y opciones para leer archivos de texto en Elixir, como la función `File.read_line/1` que permite leer una sola línea del archivo, o la opción `:encoding` que nos permite especificar la codificación del archivo.

También podemos utilizar los módulos `File` y `IO` para manipular y procesar el contenido del archivo. Por ejemplo, podemos utilizar `File.write!/2` para escribir el contenido de un archivo en otro, o `File.stat!/1` para obtener información sobre el archivo, como su tamaño o fecha de creación.

## Ver también

- Documentación oficial de Elixir sobre la lectura y escritura de archivos: https://elixir-lang.org/getting-started/io-and-the-file-system.html
- Ejemplos de uso de la función `File` en la documentación de Elixir: https://hexdocs.pm/elixir/File.html
- Tutorial sobre lectura de archivos de texto en Elixir: https://thinkingelixir.com/read-files-with-elixir/