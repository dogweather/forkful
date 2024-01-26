---
title:                "Obteniendo la fecha actual"
date:                  2024-01-20T15:13:53.793567-07:00
html_title:           "Bash: Obteniendo la fecha actual"
simple_title:         "Obteniendo la fecha actual"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/elixir/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Qué y Por Qué?
Obtener la fecha actual en Elixir significa acceder al calendario y al reloj del sistema para saber el momento presente. Los programadores necesitan esta información para tareas como registrar eventos, calcular plazos o mostrar fechas a los usuarios.

## Cómo hacerlo:
Aquí tienes un ejemplo sencillo de cómo obtener la fecha actual en Elixir:

```elixir
# Importamos el módulo DateTime
alias DateTime

# Obtenemos la fecha y hora actual en UTC
ahora = DateTime.utc_now()

# Imprimimos la fecha y hora actual
IO.puts DateTime.to_string(ahora)
```

Cuando ejecutes este código, verás algo así como la siguiente salida (la fecha y hora variarán según el momento en que se ejecute):

```
"2023-04-01T12:34:56Z"
```

## Profundización:
Históricamente, manejar fechas y horas ha sido un lío por las distintas zonas horarias y formatos. Elixir, lanzado en 2011, utiliza los módulos `DateTime`, `Date`, y `Time` del paquete `Calendar` para abordar estos temas de manera sencilla y eficaz.

Respecto a alternativas, se puede utilizar la librería `Timex`, que es una extensión de las capacidades de manipulación de fecha y hora en Elixir.

En cuanto a la implementación, `DateTime.utc_now()` obtiene la fecha y hora actual en UTC. Si necesitas la hora en una zona específica, puedes convertir la hora UTC a otra zona con `DateTime.shift_zone/3`.

## Ver También:
Aquí tienes algunos enlaces que te podrían ser útiles para profundizar:

- Documentación oficial de Elixir sobre la fecha y hora: https://hexdocs.pm/elixir/DateTime.html
- Timex, una librería Elixir rica en funciones para trabajar con fechas y horas: https://hexdocs.pm/timex/readme.html
- Zonas horarias en Elixir con el módulo `TZData`: https://hexdocs.pm/tzdata/readme.html
