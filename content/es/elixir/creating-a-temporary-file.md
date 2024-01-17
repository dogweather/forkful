---
title:                "Creando un archivo temporal"
html_title:           "Elixir: Creando un archivo temporal"
simple_title:         "Creando un archivo temporal"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/elixir/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## ¿Qué y por qué?

Crear un archivo temporal es una forma común para que los programadores manejen datos de manera temporal durante la ejecución de un programa. Esto puede ser útil para almacenar datos temporales como caché, variables temporales o para realizar operaciones específicas antes de guardar los datos en un archivo permanente.

## Cómo hacerlo:

```elixir
# Importamos el módulo "File" para manipular archivos
import File

# Creamos un archivo temporal con el prefijo "temp" y la extensión ".txt"
{:ok, temp_file} = File.temp_file("temp", ".txt")

# Escribimos un texto en el archivo temporal
write_result = File.write(temp_file, "Este es un archivo temporal.")

# Leemos el contenido del archivo
read_result = File.read(temp_file)

# Imprimimos el resultado
IO.puts(read_result)

# Cerramos y eliminamos el archivo temporal
File.close(temp_file)
File.delete(temp_file)
```

**Salida:**
```
Este es un archivo temporal.
```

## Profundizando:

Crear archivos temporales se remonta a los primeros días de la programación, cuando los recursos de almacenamiento eran limitados y se necesitaba una forma de manejar datos de manera eficiente durante la ejecución de un programa. Hoy en día, también existen otras formas de manejar datos temporales, como variables en memoria o bases de datos en memoria.

En Elixir, la función `File.temp_file/2` nos permite crear un archivo temporal en una ubicación específica o en la ubicación predeterminada del sistema, que varía según la plataforma en la que se está ejecutando el programa. Además, la función también devuelve una tupla con un átomo `:ok` y una ruta al archivo temporal creado, lo que hace que sea fácil y rápido acceder al archivo.

## Ver también:

- [Documentación oficial de File en Elixir](https://hexdocs.pm/elixir/File.html#temp_file/2) para obtener más información sobre la función `File.temp_file/2`.
- [Artículo sobre variables en memoria vs archivos temporales](https://www.geeksforgeeks.org/temporary-variables-vs-file/), que discute diferentes formas de manejar datos temporales en la programación.
- [Discusión sobre el uso de archivos temporales en programación](https://news.ycombinator.com/item?id=22282224) en la comunidad Hacker News.