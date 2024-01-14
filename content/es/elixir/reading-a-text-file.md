---
title:                "Elixir: Leyendo un archivo de texto"
simple_title:         "Leyendo un archivo de texto"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/elixir/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Por qué

Aprender a leer archivos de texto es una habilidad importante para cualquier programador de Elixir. Con esta habilidad, podrás leer y procesar grandes cantidades de datos de manera eficiente, lo que te permitirá crear aplicaciones más robustas y eficaces.

## Cómo hacerlo

Primero, necesitas crear una ruta al archivo de texto que deseas leer. Luego, puedes utilizar la función `File.open/2` para abrir el archivo y leer su contenido. Aquí hay un ejemplo de código en Elixir que muestra cómo hacerlo:

```Elixir
archivo = "ruta/al/archivo.txt"
{:ok, archivo} = File.open(archivo)

IO.puts File.read(archivo)
```

Este código primero asigna la ruta del archivo de texto a la variable `archivo` y luego utiliza la función `File.open/2` para abrir el archivo y asignarlo a la misma variable. Luego, utiliza la función `IO.puts` para imprimir el contenido del archivo en la consola.

## Profundizando

Al leer un archivo de texto en Elixir, es importante tener en cuenta el tamaño del archivo. Si el archivo es demasiado grande, leerlo y almacenarlo en la memoria puede ralentizar tu aplicación y potencialmente causar errores. Para manejar archivos grandes, puedes utilizar las funciones `IO.stream/2` y `Enum.each/2` junto con un bloque `do` en lugar de la función `IO.puts`. Esto te permite leer el archivo línea por línea y procesar cada línea de forma individual, evitando así el almacenamiento de todo el contenido del archivo en la memoria.

### Vinculación dinámica

Además de leer archivos de texto estáticos, también puedes utilizar la función `File.stream!/2` para leer archivos de forma dinámica. Esto significa que puedes pasar una ruta de archivo como un argumento y leer diferentes archivos dependiendo de las entradas del usuario. Esto es especialmente útil si estás creando una aplicación que necesita leer diferentes archivos según la ubicación del usuario o sus preferencias personales.

## Ver también

- [Documentación de Elixir sobre lectura de archivos](https://hexdocs.pm/elixir/File.html#content)
- [Artículo en español sobre procesamiento de archivos en Elixir](https://apuntes.develoteca.com/2018/08/05/procesando-archivos-de-texto-en-elixir/)