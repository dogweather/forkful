---
title:                "Elixir: Lectura de argumentos de línea de comandos"
simple_title:         "Lectura de argumentos de línea de comandos"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/elixir/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Por qué
En la programación de Elixir, a menudo es necesario leer argumentos de línea de comandos para que nuestro programa pueda tomar decisiones en función de la entrada del usuario. En esta publicación, exploraremos cómo hacerlo de manera eficiente y efectiva.

## Cómo hacerlo
Usando el módulo `OptionParser` de Elixir, podemos leer y procesar los argumentos de línea de comandos de manera muy sencilla. Primero, importamos el módulo en nuestro archivo de Elixir:

```Elixir
import OptionParser
```

Luego, definimos las opciones que queremos leer y cómo deben ser procesadas. Por ejemplo, si queremos leer dos argumentos, uno para un nombre y otro para una edad, podemos hacerlo de la siguiente manera:

```Elixir
OptionParser.parse(
  ["--nombre", "Juan", "--edad", "25"],
  switches: [
    nombre: {:string, "--nombre"},
    edad: {:integer, "--edad"}
  ]
)
```

Esto creará un mapa con la estructura `{nombre: "Juan", edad: 25}` que podemos utilizar en nuestro programa. Además, podemos definir acciones para cada opción, como imprimir un mensaje si se ingresa un argumento incorrecto:

```Elixir
OptionParser.parse(
  ["--nombre", "Juan", "--edad", "25", "--altura", "1.80"],
  switches: [
    nombre: {:string, "--nombre"},
    edad: {:integer, "--edad"},
    altura: {:float, "--altura"}
  ],
  on_argument_error: fn(option, _value) ->
    IO.puts "El argumento #{option} no fue ingresado correctamente."
  end
)

# Salida:
# El argumento --altura no fue ingresado correctamente.
```

## Deep Dive
El módulo `OptionParser` también nos permite definir opciones con argumentos opcionales y argumentos con múltiples valores. Podemos hacerlo utilizando las opciones `{:option, :optional}` y `{:option, :repeatable}`, respectivamente.

Además, podemos utilizar la opción `:switch` para definir una opción que solamente necesita ser mencionada para ser activada, sin ningún valor asociado. Por ejemplo, si queremos una opción `--ayuda` que muestre un mensaje de ayuda, podemos hacerlo de la siguiente manera:

```Elixir
OptionParser.parse(
  ["--nombre", "Juan", "--edad", "25", "--ayuda"],
  switches: [
    nombre: {:string, "--nombre"},
    edad: {:integer, "--edad"},
    ayuda: :switch
  ],
  on_switch: fn(_option) ->
    IO.puts "Bienvenido a este programa de Elixir!"
  end
)

# Salida:
# Bienvenido a este programa de Elixir!
```

## Ver también
- Documentación oficial de OptionParser: https://hexdocs.pm/elixir/OptionParser.html
- Ejemplos prácticos de lectura de argumentos de línea de comandos en Elixir:
  https://medium.com/@afe19940913/handling-command-line-arguments-in-elixir-f55f041c8711