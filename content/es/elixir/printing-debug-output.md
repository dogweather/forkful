---
title:                "Imprimiendo salida de depuración"
html_title:           "Arduino: Imprimiendo salida de depuración"
simple_title:         "Imprimiendo salida de depuración"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/elixir/printing-debug-output.md"
---

{{< edit_this_page >}}

## ¿Qué y Por qué?
La impresión de salida de depuración se refiere a la generación de información adicional que permite a los programadores rastrear y entender cómo está funcionando su código. Es útil para encontrar y solucionar errores, además de ayudar a optimizar el rendimiento de tu código.

## Cómo hacer:
En Elixir, suele utilizar la función `IO.inspect/2` para imprimir la salida de depuración. Aquí tienes un ejemplo:

```elixir
defmodule Ejemplo do
  def funcion_debug do
    data = [1,2,3,4]
    IO.inspect(data, label: "mi data")
  end
end
```
El `label:` es optativo. Si utilizas este código, verás la siguiente salida:

```elixir
mi data: [1,2,3,4]
```

## Profundizando

Elixir ha adoptado la simplicidad de Erlang para imprimir la salida de depuración, la cual ha sido de gran ayuda para los programadores de Erlang durante décadas. A su vez, existen librerías como `dbg` de Erlang y `IEx` de Elixir que dan opciones avanzadas.
Como alternativa al método `IO.inspect/2`, puedes utilizar funciones parecidas provistas por otras librerías, como `Logger.debug/2`. La ventaja de `Logger.debug/2` es que puedes dirigir tus mensajes de depuración a diferentes backends y configurarlos para que sólo se muestren bajo ciertas condiciones.
En cuanto a su implementación, `IO.inspect/2` simplemente toma tus datos y los transforma en una cadena usando el protocolo `Inspect` de Elixir, luego imprime esa cadena en la salida estándar.

## Ver también 
Para más información sobre la depuración en Elixir, consulta estas fuentes:
- [Guía oficial de Elixir](https://elixir-lang.org/getting-started/debugging.html)
- [Módulo IO.inspect](https://hexdocs.pm/elixir/IO.html#inspect/2)