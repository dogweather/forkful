---
title:                "Escribiendo un archivo de texto."
html_title:           "Elixir: Escribiendo un archivo de texto."
simple_title:         "Escribiendo un archivo de texto."
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/elixir/writing-a-text-file.md"
---

{{< edit_this_page >}}

## ¿Qué y Por Qué?

Escribir un archivo de texto es una tarea común para los programadores en Elixir. Esto implica guardar información en un archivo que puede ser leído y modificado posteriormente. Los programadores suelen hacerlo para almacenar datos o configuraciones que necesitan para sus aplicaciones.

## Cómo:

```Elixir
# Crear un archivo de texto nuevo
File.write("nuevo_archivo.txt", "¡Hola, Mundo!")

# Agregar contenido a un archivo existente
File.write("nuevo_archivo.txt", "¡Hola, de nuevo!", [:append])

# Leer contenido de un archivo
File.read("nuevo_archivo.txt")

# Obtener la fecha de modificación de un archivo
File.stat!("nuevo_archivo.txt").mtime
```

## Profundizando:

Escribir archivos de texto ha sido una tarea fundamental en la programación desde los primeros días de la informática. Antes de la popularidad de las bases de datos, los archivos de texto se utilizaban como una forma sencilla de almacenar y acceder a datos. Aunque hoy en día hay muchas opciones para almacenar y manejar datos, los archivos de texto siguen siendo una herramienta útil para los programadores.

Alternativamente a usar la función `File.write`, también se pueden escribir archivos utilizando `IO.write`, pero esta opción es menos recomendada ya que está diseñada para operar con flujos de entrada y salida.

Al escribir archivos de texto, es importante tener en cuenta la ruta del archivo para que éste se guarde en el lugar correcto. También es importante asegurarse de cerrar el archivo después de escribirlo para evitar problemas de memoria.

## Ver También:

- La documentación oficial de Elixir para la función `File.write`: https://hexdocs.pm/elixir/File.html#write/2
- Un tutorial de cómo escribir y leer archivos en Elixir: https://thoughtbot.com/blog/reading-and-writing-files-in-elixir