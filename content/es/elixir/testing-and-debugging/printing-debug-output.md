---
date: 2024-01-20 17:52:16.590140-07:00
description: "C\xF3mo Hacerlo: Elixir hace que imprimir mensajes de depuraci\xF3n\
  \ sea un juego de ni\xF1os. Puedes utilizar `IO.puts` para la salida est\xE1ndar\
  \ o `IO.inspect` que\u2026"
lastmod: '2024-03-13T22:44:58.705309-06:00'
model: gpt-4-1106-preview
summary: "Elixir hace que imprimir mensajes de depuraci\xF3n sea un juego de ni\xF1\
  os."
title: "Imprimiendo salida de depuraci\xF3n"
weight: 33
---

## Cómo Hacerlo:
Elixir hace que imprimir mensajes de depuración sea un juego de niños. Puedes utilizar `IO.puts` para la salida estándar o `IO.inspect` que devuelve el valor inspeccionado, permitiendo encadenar llamadas.

```elixir
# Para imprimir un simple mensaje:
IO.puts "Algo interesante ocurrió."

# Para inspeccionar una variable:
variable = "Hola, Elixir!"
IO.inspect variable
# Salida: "Hola, Elixir!"

# Encadenar inspección sin afectar el flujo del código:
1..5
|> Enum.map(&(&1 * 3))
|> IO.inspect(label: "después de triplicar")
|> Enum.sum()
```
Tendrás algo como esto en la salida con el segundo ejemplo:
```
después de triplicar: [3, 6, 9, 12, 15]
```

## Profundizando:
Antes de `IO.inspect`, los desarrolladores a menudo colocaban `IO.puts` en todas partes. Pero `IO.inspect` es superior ya que no altera el flujo de datos. En el mundo de Elixir, también puedes usar herramientas avanzadas de depuración como `IEx.pry` para un enfoque más interactivo, o incluso `:debugger` para una experiencia completa de depuración en la máquina virtual de Erlang.

Las herramientas de registro, como `Logger`, también son alternativas para conservar mensajes de depuración sin ensuciar la salida estándar, especialmente en producción.

Históricamente, la depuración ha sido una mezcla de arte y ciencia, ya que los primeros programadores utilizaban hasta métodos físicos (como la famosa polilla en el relé que dio origen al término "bug") para dar con problemas. En Elixir, gracias a su inmutable naturaleza y la concurrencia basada en actores, encontrar y solucionar errores puede ser menos caótico, pero sigue siendo fundamental comprender bien las herramientas a disposición.

## Ver También:
- Documentación oficial de `IO`: https://hexdocs.pm/elixir/IO.html
- Guía de inicio rápido IEx: https://hexdocs.pm/iex/IEx.html
- Elixir School sobre depuración: https://elixirschool.com/es/lessons/specifics/debugging/
- Documentación de `Logger`: https://hexdocs.pm/logger/Logger.html
