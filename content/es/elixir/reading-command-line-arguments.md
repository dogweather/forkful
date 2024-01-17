---
title:                "Leyendo argumentos de línea de comando."
html_title:           "Elixir: Leyendo argumentos de línea de comando."
simple_title:         "Leyendo argumentos de línea de comando."
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/elixir/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## ¿Qué & Por qué?

En programación, leer argumentos de la línea de comandos se refiere a obtener los valores que se proporcionan al ejecutar un programa desde la línea de comandos. Esto permite a los programadores personalizar la ejecución de sus programas de acuerdo a sus necesidades. 

Los programadores utilizan la lectura de argumentos de la línea de comandos para proporcionar una manera flexible de interactuar con sus programas, lo que facilita su uso y adaptación a diferentes situaciones y entornos.

## How to:
Elixir proporciona una manera sencilla de leer argumentos de la línea de comandos utilizando la función `System.argv`. Esta función devuelve una lista de todos los argumentos proporcionados, incluyendo el nombre del archivo que se está ejecutando como primer argumento.

```elixir
# Ejemplo de código

args = System.argv           # Obtener argumentos
IO.puts "¡Hola #{args[1]}!"  # Imprime el primer argumento
```

Para ejecutar este código, desde la línea de comandos se debe escribir `elixir hola.exs Mundo`, lo que imprimirá "¡Hola Mundo!".

## Deep Dive
La lectura de argumentos de la línea de comandos no solo se limita a Elixir, esto también es una técnica común en muchos otros lenguajes de programación. Algunas alternativas para Elixir incluyen `OptionParser` y `Getopt`, que proporcionan una manera más estructurada de manejar y validar argumentos de la línea de comandos.

La implementación de `System.argv` en Elixir utiliza la biblioteca `elixir_argv` para obtener los argumentos de la línea de comandos. Esta biblioteca a su vez utiliza la biblioteca C `getopt` para obtener los argumentos. Esto significa que la lectura de argumentos de la línea de comandos en Elixir no es compatible con Windows, ya que `getopt` solo está disponible en sistemas Unix.

## See Also
- Documentación oficial de Elixir sobre `System.argv`: https://hexdocs.pm/elixir/System.html#argv/0
- Alternativas para leer argumentos de la línea de comandos en Elixir: https://github.com/mensfeld/elixir-argv