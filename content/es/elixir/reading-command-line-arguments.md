---
title:    "Elixir: Leyendo argumentos de línea de comandos"
keywords: ["Elixir"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/es/elixir/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Por qué leer argumentos de línea de comando en Elixir

Si estás empezando a aprender Elixir o simplemente quieres mejorar tus habilidades, entender cómo leer argumentos de línea de comando es un paso importante. Esto te permitirá crear aplicaciones más dinámicas e interactivas, además de tener un mejor control sobre tu código.

## Cómo hacerlo

Para leer argumentos de línea de comando en Elixir, utilizamos la función `System.argv/0`. Esta función devuelve una lista de los argumentos pasados por línea de comando, donde el primer elemento es el nombre del script que estás ejecutando.

```Elixir
args = System.argv()
```

Puedes acceder a cada uno de los argumentos utilizando el operador de indexación `[ ]` y pasando el índice del argumento que deseas obtener.

```Elixir
first_arg = args[1]
```

A continuación, puedes utilizar estos argumentos en tu código según sea necesario. Por ejemplo, puedes utilizarlos para tomar decisiones en tu programa o para enviar información a una base de datos.

## Inmersión profunda

Además de la función `System.argv/0`, también podemos utilizar el módulo `OptionParser` para leer y validar argumentos de línea de comando en Elixir. Este módulo nos permite especificar argumentos con diferentes opciones y valores y proporciona funciones para obtenerlos fácilmente en nuestro código.

```Elixir
defmodule ArgsParser do
  def print_name(args) do
    arg_parser = OptionParser.parse(args, switches: [name: :required])
    case arg_parser do
      { _, [{:name, name}] } -> IO.puts("Tu nombre es #{name}")
      _ -> raise "Por favor, ingresa tu nombre con el argumento --name"
    end
  end
end
```

En este ejemplo, especificamos que el argumento `name` es requerido y luego lo obtenemos con la función `IO.puts/1`.

## Ver también

- Documentación de Elixir sobre la función `System.argv/0`: https://hexdocs.pm/elixir/System.html#argv/0
- Documentación de Elixir sobre el módulo `OptionParser`: https://hexdocs.pm/elixir/OptionParser.html