---
title:                "Elixir: Convirtiendo una fecha en una cadena"
programming_language: "Elixir"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/elixir/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Por qué

Muchas veces en el desarrollo de software, necesitamos manejar fechas y mostrarlas de una forma legible para el usuario. En tales casos, convertir una fecha en una cadena de texto es una tarea común. En este artículo, aprenderemos cómo hacer esto en Elixir y profundizaremos en el concepto.

## Cómo hacerlo

Para convertir una fecha en una cadena de texto en Elixir, podemos usar la función `DateTime.to_string/1`. Esta función toma como argumento una fecha en formato `DateTime`, `Date` o `Time` y devuelve una cadena de texto en formato ISO8601.

Para entender mejor, echemos un vistazo a un ejemplo utilizando la función `DateTime.utc_now/0` para obtener la hora actual en formato UTC y luego convirtiéndola en una cadena de texto:

```Elixir
DateTime.to_string(DateTime.utc_now())
```

El resultado de este código sería una cadena de texto similar a esta:

`"2021-10-12T15:43:27.422Z"`

Este formato incluye la fecha, hora, minutos, segundos y milisegundos, junto con la marca de tiempo de la zona horaria UTC.

También podemos especificar un formato personalizado utilizando la función `DateTime.to_string/2`, donde el segundo argumento es una cadena de texto que describe el formato deseado. Por ejemplo, si queremos mostrar solo la fecha en formato "aaaa-mm-dd", podemos hacer lo siguiente:

```Elixir
DateTime.to_string(DateTime.utc_now(), "{YYYY}-{MM}-{DD}")
```

Esto resultaría en una cadena de texto similar a esta:

`"2021-10-12"`

## Profundizando

En Elixir, todas las fechas y horas siguen el estándar de la biblioteca `Calendar`, que especifica la representación interna de la fecha y hora en forma de un número entero. Este número es conocido como "unix timestamp" y representa la cantidad de segundos transcurridos desde la medianoche del 1 de enero de 1970 en la zona horaria UTC.

Al convertir una fecha en una cadena de texto, lo que realmente hacemos es convertir este número en una forma legible para los humanos. Por lo tanto, es importante conocer cómo funciona esta representación interna para realizar conversiones precisas.

Además, también es importante tener en cuenta la zona horaria al convertir una fecha en una cadena de texto, ya que puede afectar el resultado final. Por ejemplo, si queremos mostrar la hora local de un usuario en lugar de la hora UTC, debemos tener en cuenta la ubicación y la configuración de la zona horaria en nuestro código.

## Ver también

- [Documentación de Elixir sobre DateTime](https://hexdocs.pm/elixir/DateTime.html)
- [Artículo sobre soporte de zonas horarias en Elixir](https://www.brianstorti.com/timezone-support-in-elixir/)