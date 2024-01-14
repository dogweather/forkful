---
title:                "Elixir: Convirtiendo una fecha en una cadena"
simple_title:         "Convirtiendo una fecha en una cadena"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/elixir/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Por qué

La conversión de una fecha a una cadena de caracteres es una habilidad fundamental en la programación Elixir, ya que permite mostrar fechas de una manera más legible y fácil de entender para los usuarios de una aplicación.

## Cómo hacerlo

Para convertir una fecha a una cadena de caracteres en Elixir, utilizamos la función `DateTime.to_string/2`. Esta función toma dos argumentos, la fecha que queremos convertir y un formato opcional. Veamos un ejemplo:

```Elixir
DateTime.to_string(~U[2021-03-21T12:00:00Z], "{0} {1}") 
# Output: "21 de Mar del 2021 12:00:00 PM UTC"
```

En este caso, estamos convirtiendo una fecha en formato universal `~U` y especificando un formato personalizado en el que la fecha se mostrará como "Día de Mes del Año Hora:Minuto:Segundo Zona Horaria".

También podemos utilizar `DateTime.to_string/4` para especificar un idioma y una zona horaria específicos. Veamos otro ejemplo:

```Elixir
DateTime.to_string(~N[2021-W12-3T12:00:00], "{D} {MMMM} {YYYY} en {h}:{m} {a}", locale: "es", timezone: "UTC") 
# Output: "17 marzo 2021 en 12:00 PM"
```

En este caso, estamos utilizando un formato de fecha específico, `~N`, y convirtiéndolo a una cadena de caracteres en español y en la zona horaria universal (`UTC`).

## Profundizando

Existen diferentes opciones de formato que podemos usar en `DateTime.to_string`, como `D` para el día, `d` para el día de la semana, `Y` para el año, `M` para el mes, `m` para el minuto, entre otros. También podemos especificar un idioma utilizando la opción `locale` y una zona horaria con la opción `timezone`.

Otra opción útil es la función `DateTime.format/3`, que nos permite especificar un formato personalizado para la fecha. Por ejemplo:

```Elixir
DateTime.format(~U[2021-03-21T12:00:00Z], "{YYYY}-{MM}-{DD}") 
# Output: "2021-03-21"
```
Esta función nos da más flexibilidad para mostrar la fecha de la manera que deseemos.

## Ver también

- Documentación oficial sobre `DateTime.to_string`: https://hexdocs.pm/elixir/DateTime.html#to_string/2
- Ejemplos de formatos de fecha en Elixir: https://www.electrictoolbox.com/formatting-dates-elixir/