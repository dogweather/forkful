---
date: 2024-01-26 04:13:30.532208-07:00
description: "C\xF3mo hacerlo: Elm no viene con un REPL integrado. Sin embargo, puedes\
  \ usar `elm repl` desde tu l\xEDnea de comandos para iniciar una sesi\xF3n de Elm\
  \ despu\xE9s de\u2026"
lastmod: '2024-03-13T22:44:58.987210-06:00'
model: gpt-4-0125-preview
summary: Elm no viene con un REPL integrado.
title: Usando una shell interactiva (REPL)
weight: 34
---

## Cómo hacerlo:
Elm no viene con un REPL integrado. Sin embargo, puedes usar `elm repl` desde tu línea de comandos para iniciar una sesión de Elm después de instalar Elm.

```Elm
> importar Lista exponiendo (..)
> mapa (\x -> x * 2) [1, 2, 3, 4]
[2,4,6,8] : Lista número
```

En esta sesión, después de importar las funciones de Lista, duplicamos los números en una lista y obtuvimos el resultado instantáneamente.

## Análisis profundo
El REPL de Elm puede parecer limitado en comparación con los de otros lenguajes como Python o JavaScript, ya que Elm es un lenguaje compilado centrado en producir aplicaciones web. Históricamente, Elm se ha centrado en aplicaciones completas en lugar de scripting o interacciones con shell.

Alternativas al REPL de Elm incluyen `elm-live` y editores en línea como Ellie donde puedes ver los cambios en el código reflejados en tiempo real en un navegador.

En cuanto a la implementación, el REPL de Elm compila fragmentos de código de Elm en JavaScript en segundo plano, permitiéndote ejecutar Elm de manera interactiva. Esto es diferente de los REPL de lenguajes interpretados, que no necesitan este paso de compilación. El REPL de Elm también está simplificado para mantener el lenguaje básico ligero y enfocado.

## Ver también
- La guía oficial de Elm sobre interactividad: https://guide.elm-lang.org/interop/
- Ellie, un patio de juegos en línea de Elm: https://ellie-app.com/new
- `elm-live`, un servidor de desarrollo flexible para Elm: https://www.elm-live.com/
