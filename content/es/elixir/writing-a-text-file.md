---
title:                "Escritura de un archivo de texto"
date:                  2024-01-19
simple_title:         "Escritura de un archivo de texto"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/elixir/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Qué y Por Qué?
Escribir un archivo de texto en Elixir significa guardar datos en un archivo que puedes leer y modificar fácilmente. Los programadores lo hacen para mantener registros, exportar datos o configurar programas con archivos .txt o .csv.

## Cómo hacerlo:
```elixir
# Crea un archivo y escribe un mensaje
File.write!("saludo.txt", "Hola, esto es Elixir")

# Añade contenido al archivo existente
File.write!("saludo.txt", "\n¡Escribiendo archivos es fácil!", [:append])

# Lee y muestra el contenido del archivo
IO.puts(File.read!("saludo.txt"))
```
Salida del ejemplo:
```
Hola, esto es Elixir
¡Escribiendo archivos es fácil!
```

## Inmersión Profunda
Históricamente, Elixir usa la abstracción del módulo `File` que proviene de la Erlang VM. Alternativas incluyen el uso de Ecto para trabajar con bases de datos o GenServer para manejar estado. Al escribir archivos, considera el manejo de errores y concurrencia, ya que Elixir es excelente para tareas paralelas.

## Ver También
- [Elixir School - Files](https://elixirschool.com/en/lessons/basics/collections/)
- [Documentación oficial de Elixir - File](https://hexdocs.pm/elixir/File.html)
