---
title:                "Leyendo un archivo de texto"
html_title:           "Elixir: Leyendo un archivo de texto"
simple_title:         "Leyendo un archivo de texto"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/elixir/reading-a-text-file.md"
---

{{< edit_this_page >}}

## ¿Por qué leer un archivo de texto?

Leer y trabajar con archivos de texto es una habilidad importante en cualquier lenguaje de programación, ya que es común tener que procesar archivos de texto en nuestras aplicaciones. Además, Elixir ofrece algunas características únicas para la manipulación de archivos de texto, lo que lo hace una herramienta útil para aprender.

## ¿Cómo hacerlo?

Para leer un archivo de texto en Elixir, utilizamos la función `File.read/1`, que toma como argumento el nombre del archivo que queremos leer. Por ejemplo, si tenemos un archivo llamado "ejemplo.txt" en nuestro directorio actual, podemos leerlo de la siguiente manera:

```elixir
texto = File.read("ejemplo.txt")

# Output:
{:ok, "Este es un ejemplo de archivo de texto"}
```

La función `File.read/1` devuelve una tupla con el átomo `:ok` y el contenido del archivo como una cadena de texto. Si el archivo no existe, la función devuelve `{:error, :enoent}`.

Podemos especificar el modo en que queremos abrir el archivo utilizando el segundo argumento de la función. Por ejemplo, si queremos leer el archivo en modo binario, podemos hacerlo de la siguiente manera:

```elixir
texto = File.read("ejemplo.txt", [:binary])
```

También podemos utilizar el operador de pipe `|>` para encadenar funciones. Por ejemplo, podemos leer y imprimir el contenido de un archivo en una sola línea de código:

```elixir
"ejemplo.txt" |> File.read |> IO.puts

# Output:
Este es un ejemplo de archivo de texto
```

## Inmersión profunda

Elixir ofrece varias funciones para leer y manipular archivos de texto de manera eficiente. Por ejemplo, podemos utilizar `IO.binstream/2` para leer y escribir en archivos binarios de forma secuencial, o `File.stream!/2` para leer archivos de forma perezosa (lazy).

También podemos especificar el número de bytes que queremos leer utilizando la función `IO.inspect/2`. Esto puede ser útil para leer grandes archivos de manera más eficiente y sin ocupar demasiada memoria.

Es importante tener en cuenta que Elixir utiliza un sistema de recolección de basura, por lo que no es necesario preocuparse por cerrar los archivos después de leerlos. El recolector de basura se encargará de eso por nosotros.

## Ver también

- La documentación oficial de Elixir sobre la función `File.read/1`
(https://hexdocs.pm/elixir/File.html#read/1)
- Un tutorial sobre la manipulación de archivos en Elixir
(https://elixircasts.io/working-with-files-in-elixir)
- El libro "Programming Elixir" de Dave Thomas, que aborda el tema de archivos de texto en profundidad.