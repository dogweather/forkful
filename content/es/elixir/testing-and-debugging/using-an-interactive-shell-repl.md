---
date: 2024-01-26 04:13:02.044311-07:00
description: "C\xF3mo hacerlo: Para lanzar IEx, abre tu terminal y escribe `iex`.\
  \ Aqu\xED tienes un adelanto."
lastmod: '2024-03-13T22:44:58.704112-06:00'
model: gpt-4-0125-preview
summary: Para lanzar IEx, abre tu terminal y escribe `iex`.
title: Usando una shell interactiva (REPL)
weight: 34
---

## Cómo hacerlo:
Para lanzar IEx, abre tu terminal y escribe `iex`. Aquí tienes un adelanto:

```Elixir
iex> nombre = "Programador de Elixir"
"Programador de Elixir"
iex> String.length(nombre)
17
iex> Enum.map([1, 2, 3], fn num -> num * 3 end)
[3, 6, 9]
```

La salida debería mostrar la asignación de variables, resultados de funciones y una función anónima en acción.

## Profundización
La shell IEx ha sido parte de Elixir desde sus primeros días. José Valim, el creador de Elixir, se inspiró en las shells interactivas de otros lenguajes como el `python` de Python y el `irb` de Ruby. Aunque IEx comparte muchas características con estos, está diseñado para manejar la naturaleza concurrente de Elixir y está completamente integrado con las capacidades de la VM de Erlang.

Las alternativas a IEx en el ecosistema de Erlang incluyen `erl`, la shell de Erlang. Pero IEx ofrece un entorno más amigable para Elixir, con características como completación de tabulador comprehensiva, historial y ayudantes.

La REPL IEx es más que un área de juegos; puede conectarse sin problemas a un sistema en ejecución. Esto es crucial para depurar aplicaciones en vivo. La implementación subyacente se basa en el BEAM (la VM de Erlang), asegurando que características como el intercambio de código en caliente estén soportadas directamente en la shell.

## Ver También
Consulta estos para más lectura y recursos:

- [Documentación de IEx de Elixir](https://hexdocs.pm/iex/IEx.html)
- [Elixir Interactivo (IEx) - La Shell de Elixir](https://elixir-lang.org/getting-started/introduction.html#interactive-elixir)
- [Documentación de `erl` de Erlang](http://erlang.org/doc/man/erl.html)
- [Aprendiendo la Shell Interactiva de Elixir](https://elixirschool.com/en/lessons/basics/iex_helpers/)
