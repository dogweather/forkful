---
title:                "Lectura de argumentos de línea de comandos"
aliases: - /es/elixir/reading-command-line-arguments.md
date:                  2024-01-20T17:55:53.825957-07:00
model:                 gpt-4-1106-preview
simple_title:         "Lectura de argumentos de línea de comandos"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/elixir/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Qué y Por Qué?
Leer argumentos de la línea de comandos permite que tus programas de Elixir reciban información externa al ser ejecutados, habilitándote para construir aplicaciones dinámicas que se adapten según los insumos del usuario. Los programadores usan esto para personalizar la ejecución o configurar opciones sin tener que cambiar el código.

## Cómo Hacerlo:
Elixir hace que leer argumentos de la línea de comandos sea sencillo con `System.argv/0`. Ejemplo simple:

```elixir
defmodule CLIApp do
  def main do
    args = System.argv()
    IO.inspect(args)
  end
end

# Para ejecutarlo: elixir mi_app.exs arg1 arg2 arg3
# Salida: ["arg1", "arg2", "arg3"]
```

Si quieres algo más elaborado, usa la librería `OptionParser`:

```elixir
defmodule CLIApp do
  def main do
    {opts, args, _} = OptionParser.parse(System.argv())
    
    IO.inspect(opts) # Opciones con flag, como --name
    IO.inspect(args) # Argumentos posicionales restantes
  end
end

# Ejecuta: elixir mi_app.exs --name Pepito arg1
# Salida: [name: "Pepito"]
#         ["arg1"]
```

## Más Información:
Históricamente, leer argumentos de línea de comandos es algo que proviene de los primeros días de los sistemas UNIX, permitiendo a los programas comportarse de manera flexible. Elixir, al funcionar sobre la Erlang VM, hereda esta posibilidad y la simplifica.

Alternativas a `OptionParser` podrían ser libs externas como `Clam`, que ofrecen aún más funcionalidades, como validaciones y generación de ayuda.

Detalles de implementación en Elixir incluyen la conversión automática de argumentos recibidos a listas de strings (string lists), y la capacidad de manejar opciones con múltiples valores usando `OptionParser`.

## Ver También:
- [Documentación de Elixir - System.argv](https://hexdocs.pm/elixir/System.html#argv/0)
- [Documentación de Elixir - OptionParser](https://hexdocs.pm/elixir/OptionParser.html)
