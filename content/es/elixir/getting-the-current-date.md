---
title:                "Elixir: Obteniendo la fecha actual"
programming_language: "Elixir"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/elixir/getting-the-current-date.md"
---

{{< edit_this_page >}}

## ¿Por qué necesitas obtener la fecha actual en Elixir?

Hay muchas razones por las cuales un programador Elixir puede necesitar obtener la fecha actual. Algunos ejemplos incluyen registrar la fecha en que se creó un documento, calcular la edad de una persona o simplemente mostrar la fecha actual en una aplicación web.

## Cómo obtener la fecha actual en Elixir

Obtener la fecha actual en Elixir es muy sencillo gracias a la función `:calendar.local_time`. Esta función toma como argumentos el formato de fecha deseado y devuelve una tupla con el año, mes, día, hora, minuto y segundo actuales.

```Elixir
fecha_actual = :calendar.local_time(:local)
```
Este código devolverá una tupla en el siguiente formato: `{año, mes, día, hora, minutos, segundos}`.

Para convertir esta tupla en un string legible, podemos usar la función `:calendar.format` y especificar el formato deseado. Por ejemplo, si queremos obtener la fecha y hora completa en formato de 12 horas, podemos usar el formato `"{fecha} {horaAMPM}"`.

```Elixir
fecha_actual = :calendar.local_time(:local)
fecha_formateada = :calendar.format("{fecha} {horaAMPM}", fecha_actual)
```

La variable `fecha_formateada` ahora contendrá un string como este: `"Julio 24, 2021 5:30 PM"`.

## Profundizando en la obtención de la fecha actual en Elixir

Además de las funciones `:calendar.local_time` y `:calendar.format`, Elixir también tiene otras funciones útiles para trabajar con fechas y horas. Por ejemplo, `:calendar.universal_time` devuelve la fecha y hora en formato UTC, mientras que `:calendar.datetime_to_gregorian_days` convierte una fecha en una cantidad de días en el calendario gregoriano.

También es importante tener en cuenta que estas funciones dependen del sistema operativo en el que se esté ejecutando el código, por lo que puede haber pequeñas variaciones en los resultados.

## Ver también

- [Guía oficial de fechas y horas en Elixir](https://hexdocs.pm/elixir/Calendar.html)
- [Artículo sobre la manipulación de fechas en Elixir](https://digitalflap.com/manipulating-dates-and-times-in-elixir/)