---
title:    "Gleam: Convirtiendo una fecha en una cadena"
keywords: ["Gleam"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/es/gleam/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Por qué

Converting a date into a string (convertir una fecha en una cadena) is a common task in programming, especially when working with databases or user interfaces. In Gleam, this can be achieved with a few simple steps. Converting dates into strings allows for easier manipulation and presentation of data, making it a useful skill to have for any developer.

## Cómo hacerlo

```Gleam
let date = Time.now() // fecha actual
let string_date = Time.format(date, "%d/%m/%Y") // convertir fecha en cadena con el formato día/mes/año
```

En el código de ejemplo anterior, utilizamos la función `format` de la biblioteca de fecha y hora de Gleam para convertir la fecha en una cadena. El primer argumento es la fecha que queremos convertir, mientras que el segundo argumento es el formato en el que queremos que se muestre. En este caso, utilizamos `%d` para el día, `%m` para el mes y `%Y` para el año. 

Si queremos mostrar la hora junto con la fecha, también podemos utilizar `%H` para las horas, `%M` para los minutos y `%S` para los segundos. Por ejemplo:

```Gleam
let time = Time.from(12, 30, 0) // crear una nueva fecha y hora con las horas, minutos y segundos especificados
let string_time = Time.format(time, "%H:%M:%S") // convertir la fecha y hora en cadena con el formato horas:minutos:segundos
```

La biblioteca también ofrece otras opciones de formato, como el nombre del mes completo `%B` o el día de la semana `%A`. Puedes consultar la documentación de la biblioteca para ver todas las opciones disponibles.

## Profundizando

Internamente, las fechas en Gleam se representan como una estructura de datos llamada `Time.DateTime` que contiene información sobre el año, mes, día, hora, minutos y segundos. Ver la fecha y hora de esta manera puede ser útil para realizar cálculos y manipulaciones con fechas. Sin embargo, cuando se trata de presentar las fechas a los usuarios, convertirlas en cadenas es más práctico.

Una cosa a tener en cuenta es que al convertir una fecha en una cadena, es importante utilizar el formato correcto. Por ejemplo, si usamos `%m` para el mes y la fecha es 2 de enero, obtendremos `"01"` en lugar de `"1"`. Esto puede ser confuso para los usuarios, así que asegúrate de revisar los formatos y elegir el más adecuado para tu caso de uso.

## Ver también

- [Gleam - Biblioteca de fecha y hora](https://gleam.run/modules/time)
- [Gleam - Documentación de la biblioteca de fecha y hora](https://gleam.run/modules/time#to_string)
- [Convertir una fecha en una cadena en otros lenguajes de programación](https://www.programacion.com.py/escritorio/python/convertir-fecha-en-cadena-python)