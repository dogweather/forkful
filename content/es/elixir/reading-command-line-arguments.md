---
title:                "Leyendo argumentos de línea de comando"
html_title:           "Elixir: Leyendo argumentos de línea de comando"
simple_title:         "Leyendo argumentos de línea de comando"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/elixir/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## ¿Por qué leer argumentos de línea de comando?

La lectura de argumentos de línea de comando es una habilidad esencial para cualquier programador de Elixir. Al comprender cómo funcionan los argumentos de línea de comando, podrás hacer que tus programas sean más flexibles y adaptables, lo que mejora la experiencia del usuario.

## Cómo hacerlo

Para leer argumentos de línea de comando en Elixir, puedes utilizar el módulo `System` y su función `argv`. Esto te devolverá una lista con los argumentos ingresados al ejecutar el programa.

Veamos un ejemplo de cómo utilizarlo:

```Elixir
# Este programa toma un nombre como argumento de línea de comando
defmodule CommandLine do
  def main do
    args = System.argv
    nombre = Enum.at(args, 0)

    IO.puts "¡Hola #{nombre}!"
  end
end

CommandLine.main()
```

Si ejecutas este programa desde la línea de comando escribiendo `elixir hello.exs World`, la salida será:

```
¡Hola World!
```

Además de esto, también puedes utilizar la función `get_env` del módulo `System` para acceder a las variables de entorno, que pueden ser pasadas como argumentos al ejecutar el programa.

## Profundizando

La función `argv` es útil, pero tiene algunas limitaciones. Por ejemplo, si los argumentos contienen espacios, estos se interpretarán como argumentos separados. Para solucionar esto, puedes utilizar el módulo `OptionParser` que permite parsear argumentos de manera más flexible.

Además, al utilizar la función `argv` también se obtienen otros argumentos del sistema, como la ruta del archivo ejecutado y la bandera `--no-halt`, que indica si se debe finalizar la ejecución de manera forzada. Si no quieres obtener estos argumentos, puedes utilizar la función `command_line` del módulo `Kernel` para acceder solo a los argumentos específicos de tu programa.

## Ver también

- Documentación del módulo [System](https://hexdocs.pm/elixir/System.html)
- Documentación del módulo [OptionParser](https://hexdocs.pm/elixir/OptionParser.html)
- Ejemplos de [argumentos de línea de comando en Elixir](https://github.com/dwyl/learn-elixir/blob/master/command-line-arguments.md)