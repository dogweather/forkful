---
title:                "Usando una shell interactiva (REPL)"
date:                  2024-01-26T04:13:30.532208-07:00
model:                 gpt-4-0125-preview
simple_title:         "Usando una shell interactiva (REPL)"
programming_language: "Elm"
category:             "Elm"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/elm/using-an-interactive-shell-repl.md"
---

{{< edit_this_page >}}

## ¿Qué y por qué?
El Bucle Leer-Evaluar-Imprimir (REPL, por sus siglas en inglés) es un entorno de programación interactivo y sencillo que toma entradas individuales del usuario, las evalúa y devuelve el resultado al usuario. Los programadores de Elm utilizan el REPL para experimentos rápidos, depuración o aprendizaje del lenguaje.

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