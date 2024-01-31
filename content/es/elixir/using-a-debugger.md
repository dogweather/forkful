---
title:                "Usando un depurador"
date:                  2024-01-26T03:48:15.380274-07:00
model:                 gpt-4-0125-preview
simple_title:         "Usando un depurador"

tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/elixir/using-a-debugger.md"
---

{{< edit_this_page >}}

## ¿Qué y Por Qué?
Usar un depurador en Elixir implica recorrer tu código paso a paso, inspeccionar variables y seguir flujos para eliminar errores. Los programadores lo hacen para dar sentido a lo inesperado y asegurarse de que sus aplicaciones se comporten como están diseñadas.

## Cómo hacerlo:
Elixir viene con un depurador gráfico incorporado llamado `:debugger`. Para usarlo, necesitarás iniciarlo y adjuntarlo a tu proceso en ejecución.

Primero, asegúrate de haber iniciado `:debugger` dentro de una sesión de `iex`:
```elixir
iex> :debugger.start()
{:ok, #PID<0.108.0>}
```

Ahora, interpreta el módulo de código que deseas depurar:
```elixir
iex> :int.ni(MyApp.MyModule)
{:module, MyApp.MyModule}
```

Puedes establecer un punto de interrupción:
```elixir
iex> :int.break(MyApp.MyModule, número_de_línea)
:ok
```

Y luego, ejecuta tu función para alcanzar el punto de interrupción y avanzar a través de tu código:
```elixir
iex> MyApp.MyModule.mi_función(arg1, arg2)
# El depurador pausará la ejecución en la línea con el punto de interrupción
```

## Estudio Profundo
Antes del `:debugger` de Elixir, Erlang proporcionó el depurador que usa Elixir; es robusto y excelente en el manejo de procesos concurrentes, un punto fuerte de la VM de Erlang (BEAM). A diferencia de algunos otros depuradores, `:debugger` no permite la modificación de variables al vuelo, debido a la naturaleza inmutable de los datos en Elixir. En cuanto a alternativas, tienes `IEx.pry` que te permite pausar la ejecución y saltar a un REPL en cualquier punto de tu código, lo que puede ser muy útil.

Mientras que `:debugger` es bueno para una interfaz gráfica, algunos podrían preferir la herramienta incorporada `:observer` que también ofrece inspección de procesos y métricas del sistema, aunque no está específicamente dirigida a avanzar a través del código. La comunidad de Elixir también contribuye con herramientas como `visualixir` y `rexbug`, expandiendo el ecosistema de herramientas de depuración más allá de las predeterminadas.

## Ver También
- Guía Oficial para Empezar con Elixir sobre Depuración: https://elixir-lang.org/getting-started/debugging.html
- Documentación de `:debugger` de Erlang: http://erlang.org/doc/apps/debugger/debugger_chapter.html
- Discusiones en el Foro de Elixir sobre Técnicas de Depuración: https://elixirforum.com/c/elixir-questions/elixir-questions-questions-help/15
