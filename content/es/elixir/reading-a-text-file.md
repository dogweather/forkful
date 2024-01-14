---
title:    "Elixir: Leyendo un archivo de texto"
keywords: ["Elixir"]
---

{{< edit_this_page >}}

## ¿Por qué leer un archivo de texto en Elixir?

Los archivos de texto son una forma común de almacenar y compartir información en una variedad de aplicaciones. Al aprender a leer archivos de texto en Elixir, tendrás una herramienta poderosa para manipular y procesar grandes cantidades de datos en tus proyectos. En este artículo, te enseñaremos cómo abrir y leer un archivo de texto usando Elixir.

## Cómo leer un archivo de texto en Elixir

Para abrir un archivo de texto en Elixir, primero necesitamos crear un archivo llamado "text.txt" que contenga algunas líneas de texto. Luego, en nuestro código de Elixir, usaremos la función `File.open/2` para abrir el archivo y especificar el modo de lectura:

```elixir
File.open("text.txt", [:read]) 
```

Esto nos devolverá un proceso de Elixir que contiene el contenido del archivo. Ahora, podemos usar la función `IO.read/1` para leer y mostrar el contenido del archivo en la consola:

```elixir
content = IO.read(file)
IO.puts content
```

Si queremos leer el archivo línea por línea, podemos usar la función `IO.gets/1`, que nos devolverá cada línea del archivo como una cadena:

```elixir
IO.gets(file) # primera línea
IO.gets(file) # segunda línea
```

## Profundizando en la lectura de archivos de texto

Al abrir un archivo de texto en Elixir, debemos tener en cuenta que el contenido se devolverá como una cadena de caracteres. Si queremos trabajar con el contenido como una lista de líneas, podemos usar la función `String.split/2` para dividir la cadena en una lista de cadenas:

```elixir
file_content = IO.read(file)
lines = String.split(file_content, "\n")
```

También podemos especificar un separador diferente si nuestro archivo no usa saltos de línea como separadores. Además, al leer un archivo grande, es importante utilizar funciones de Elixir como `File.stream!/3` y `Enum.each/2` en lugar de cargar todo el contenido en memoria de una vez.

¡Ahora estás listo para leer y manipular archivos de texto en tus proyectos de Elixir con confianza!

## Ver también

- [Documentación de Elixir sobre lectura de archivos](https://elixir-lang.org/getting-started/typespecs-and-behaviours.html#types-aspec)
- [Guía de Elixir para trabajar con archivos](https://elixir-lang.org/getting-started/io-and-the-file-system.html#interacting-with-the-file-system)
- [Ejemplos de código de Elixir para leer archivos](https://github.com/elixir-lang/elixir/blob/master/lib/elixir/guards.ex)