---
title:    "Elixir: Leyendo argumentos de la línea de comandos"
keywords: ["Elixir"]
---

{{< edit_this_page >}}

## Por qué

A veces, cuando trabajamos en un programa Elixir, es necesario proporcionar ciertos parámetros al momento de ejecutarlo. Esto permite personalizar la ejecución del programa según nuestras necesidades. ¡Es hora de aprender cómo leer argumentos de línea de comandos en Elixir!

## Cómo hacerlo

Para leer argumentos de línea de comandos, utilizamos `System.argv/0`, una función integrada en Elixir. Esta función devuelve una lista con los argumentos pasados al momento de ejecutar el programa en la terminal.

Veamos un ejemplo de código:

```Elixir
defmodule Args do
  def leer_args do
    args = System.argv()

    IO.puts "Se ingresaron #{length(args)} argumentos"

    Enum.each(args, fn arg ->
      IO.puts arg
    end)
  end
end
```

Si ejecutamos este programa desde la terminal con `elixir args.ex arg1 arg2`, obtendremos la siguiente salida:

```
Se ingresaron 3 argumentos
args.ex
arg1
arg2
```

## Inmersión Profunda

Además de la función `System.argv/0`, también podemos utilizar la función `OptionParser.parse/2` para parsear argumentos de línea de comandos y asignarlos a variables. Esta función toma dos argumentos: una lista de opciones y una lista de argumentos recibidos.

Veamos un ejemplo:

```Elixir
defmodule Parser do
  def parse(args) do
    options = [
      define: :metadata,
      short: "-d",
      long: "--data",
      description: "Metadata del archivo",
      type: :string
    ]

    OptionParser.parse(args, options)
  end
end
```

Si ejecutamos este programa con `elixir parser.ex -d "info"`, obtendremos el siguiente resultado:

```
{[metadata: "info"], []}
```

## Ver También

- [Argumentos de Línea de Comandos en Elixir](https://elixir-lang.org/getting-started/running-tests.html)
- [Documentación de System.argv/0](https://hexdocs.pm/elixir/System.html#argv/0)
- [Documentación de OptionParser.parse/2](https://hexdocs.pm/elixir/OptionParser.html#parse/2)