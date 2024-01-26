---
title:                "Escribiendo en el error estándar"
html_title:           "Arduino: Escribiendo en el error estándar"
simple_title:         "Escribiendo en el error estándar"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/elixir/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## ¿Qué & Por Qué?
Escribir en el error estándar (stderr) permite mostrar mensajes de error y diagnósticos sin mezclarlos con la salida estándar (stdout). Los programadores lo hacen para facilitar el depurado y el manejo de errores, además de permitir su reutilización en otros programas o archivos.

## Cómo Hacerlo:
```elixir
# Escribe un mensaje de error en stderr
IO.puts(:stderr, "¡Error encontrado!")

# Ejemplo usando `IO.warn/2` que también escribe en stderr pero con una convención para advertencias.
IO.warn("Esto es una advertencia.")
```
Salida esperada en stderr:
```
¡Error encontrado!
Esto es una advertencia.
```
## Inmersión Profunda:
Historia: El concepto de separar la salida estándar de errores data de los primeros días de Unix para mejorar el manejo de la información en la terminal.

Alternativas: Podrías escribir errores a un archivo de log, usar `Logger` para diferentes niveles de log, o capturar stderr en pruebas unitarias.

Detalles de implementación: En Elixir, `IO.puts/2` con el primer argumento `:stderr` fuerza la escritura en stderr. `IO.warn/2` está pensado para advertencias y utiliza stderr por defecto.

## Ver También:
- Documentación oficial de `IO` en Elixir: https://hexdocs.pm/elixir/IO.html
- Introducción a Elixir para la entrada/salida: https://elixir-lang.org/getting-started/io-and-the-file-system.html
- Guía de Logging en Elixir: https://hexdocs.pm/logger/Logger.html
