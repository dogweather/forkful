---
title:                "Usando una shell interactiva (REPL)"
date:                  2024-01-26T04:14:24.572410-07:00
model:                 gpt-4-0125-preview
simple_title:         "Usando una shell interactiva (REPL)"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/gleam/using-an-interactive-shell-repl.md"
---

{{< edit_this_page >}}

## Qué y Por Qué?

Un REPL, abreviatura de Read-Eval-Print Loop (Bucle de Leer-Evaluar-Imprimir), es una herramienta de programación para ejecutar código de manera interactiva y ver resultados al instante. Los programadores lo usan para experimentar, depurar o aprender un nuevo idioma sobre la marcha como Gleam.

## Cómo:

Actualmente, Gleam no incluye un REPL dentro de su distribución estándar. Sin embargo, puedes experimentar con código Gleam utilizando la shell de Erlang existente porque Gleam se compila a bytecode de Erlang. Aquí te mostramos cómo:

1. Compila tu código Gleam a Erlang.
```plaintext
gleam build
```

2. Inicia la shell de Erlang.
```plaintext
erl -pa ebin
```

3. Llama a tus funciones de Gleam (asumiendo que tienes un módulo llamado `my_mod` y una función `my_fun`).
```erlang
my_mod:my_fun().
```

Deberías ver la salida de tu función mostrada en la shell.

## Inmersión Profunda

REPL encarna el espíritu dinámico y exploratorio de muchos lenguajes de programación funcional, remontándose al REPL de LISP en los años 60. En comparación, otros sistemas como `ipython` de Python o `irb` de Ruby ofrecen experiencias similares para sus comunidades.

Aunque Gleam no tiene un REPL nativo todavía, aprovechar la shell de Erlang sigue siendo una solución ingeniosa. Las capacidades de la shell de Erlang provienen de la BEAM VM, la máquina virtual que alimenta el ecosistema de Erlang, que incluye a Elixir, LFE y Gleam.

Las alternativas a los REPLs en el ecosistema de Gleam podrían incluir escribir casos de prueba o usar compiladores en línea y campos de juegos de código que admitan Gleam, para probar fragmentos de código fuera de una configuración de proyecto completa.

La implementación de un REPL dedicado para Gleam enfrenta desafíos principalmente alrededor de la naturaleza compilada de Gleam y el tiempo de ejecución de Erlang, donde el intercambio de código en caliente es la norma. Cualquier REPL de Gleam futuro necesitaría reconciliar la tipificación estática del lenguaje con el entorno de ejecución dinámico que un REPL espera.

## Ver También

- Documentación oficial de Gleam: https://gleam.run/book/
- Documentación de la shell de Erlang: http://erlang.org/doc/man/erl.html
- Un compilador de Gleam en línea y campo de juegos: https://gleam.run/compiler/
