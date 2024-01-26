---
title:                "Usando un depurador"
date:                  2024-01-26T03:48:55.940588-07:00
model:                 gpt-4-0125-preview
simple_title:         "Usando un depurador"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/gleam/using-a-debugger.md"
---

{{< edit_this_page >}}

## ¿Qué y Por Qué?
Usar un depurador es básicamente actuar como detective en tu código, buscando errores y averiguando por qué las cosas no funcionan de manera fluida. Los programadores lo hacen porque, enfrentémoslo, los errores son inevitables, y eliminarlos eficientemente significa que tu código funcionará más rápido y de manera más fiable.

## Cómo:
Gleam actualmente se apoya en el ecosistema de Erlang para herramientas, así que típicamente depurarás con herramientas como `rebar3`, `observer` y `debugger`. Aquí te mostramos cómo meterte de lleno en la depuración:

```gleam
// En tu configuración de rebar, asegúrate de tener estas líneas para incluir información de depuración:
{erl_opts, [debug_info]}.

// Ejecuta una shell de Erlang con tu aplicación cargada
rebar3 shell

// Dentro de la shell, puedes iniciar el depurador
1> debugger:start().
```

¿Simple, verdad? La GUI del `debugger` aparece, y puedes establecer puntos de interrupción, avanzar paso a paso por el código y observar variables tanto como desees. No verás directamente el código Gleam, sino el código Erlang al que se compila, lo cual sigue siendo bastante útil.

## Inmersión Profunda
Gleam es un lenguaje joven, así que mientras se apoya en los hombros del ecosistema de Erlang, las herramientas nativas de depuración de Gleam aún no están en el centro de atención. Eso significa que estamos utilizando las herramientas probadas y verdaderas de Erlang, y eso no es algo malo. El depurador de Erlang existe desde los años 90, perfeccionado a lo largo de años de erradicar errores molestos en sistemas donde la fiabilidad es clave.

En cuanto a alternativas, el rastreo es un método poderoso en el mundo BEAM (esa es la máquina virtual que ejecuta código de Erlang y Elixir). Usando `rebar3` puedes acceder a herramientas como `recon` para rastrear llamadas a funciones y profundizar en problemas de rendimiento.

El cambio entre escribir Gleam y depurar en Erlang puede sentirse como si estuvieras traduciendo tus pensamientos al vuelo. Pero la ventaja es que obtienes una visión del mundo de Erlang, comprendiendo los bloques de construcción de tu aplicación en su forma de ejecución.

## Ver También
Para expandir tu conjunto de herramientas de depuración, consulta:

- Documentación del depurador de Erlang: [https://erlang.org/doc/apps/debugger/debugger_chapter.html](https://erlang.org/doc/apps/debugger/debugger_chapter.html)
- La biblioteca `recon` para Erlang: [https://ferd.github.io/recon/](https://ferd.github.io/recon/)
- Sobre el rastreo en el BEAM: [https://adoptingerlang.org/docs/development/tracing/](https://adoptingerlang.org/docs/development/tracing/)