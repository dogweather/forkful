---
title:    "Elixir: Comparando dos fechas"
keywords: ["Elixir"]
---

{{< edit_this_page >}}

## Por qué

Comparar dos fechas es una tarea común en programación, especialmente cuando se trabaja con sistemas de reserva, calendarios o informes de ventas. Aprender cómo comparar fácilmente dos fechas en Elixir puede ahorrar tiempo y evitar errores en su código.

## Cómo hacerlo

Comparar dos fechas en Elixir es sencillo gracias al módulo `Date` y su función `compare/2`. Tome como ejemplo estas dos fechas:

```Elixir
date1 = ~D[2020-01-01]
date2 = ~D[2019-12-01]
```

Utilizando la función `compare/2`, podemos comparar ambas fechas de la siguiente manera:

```Elixir
compare = Date.compare(date1, date2)
```

El resultado de `compare/2` será un número entero que indica la relación entre las dos fechas, donde `-1` significa que la primera fecha es anterior a la segunda, `1` significa que la primera fecha es posterior a la segunda y `0` significa que ambas fechas son iguales.

Para una comparación más específica, también se puede utilizar la función `Date.before?/2` o `Date.after?/2` para determinar si una fecha es anterior o posterior a otra.

## Profundizando

Si desea comparar no solo las fechas sino también las horas, minutos y segundos, puede utilizar el módulo `DateTime` en lugar del módulo `Date`. Este módulo contiene las mismas funciones que `Date` pero también tiene en cuenta la hora.

En caso de necesitar manipular las fechas, se puede utilizar el módulo `Calendar` para realizar operaciones como sumar o restar días, meses o años.

## Ver también

- Documentación oficial de Elixir sobre el módulo `Date`: https://hexdocs.pm/elixir/Date.html
- Tutorial sobre cómo manipular fechas y horas en Elixir: https://cult.honeypot.io/elixir-date-datetime/
- Guía práctica para trabajar con fechas en Elixir: https://itnext.io/date-time-and-timezones-in-elixir-160dc7f94d9f