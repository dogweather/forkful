---
title:                "Gleam: Comparando dos fechas"
simple_title:         "Comparando dos fechas"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/gleam/comparing-two-dates.md"
---

{{< edit_this_page >}}

# ¿Por qué comparar dos fechas en Gleam?

Comparar dos fechas puede ser una tarea común al trabajar con datos temporales en Gleam. Esto puede ayudarte a determinar la diferencia en tiempo entre dos eventos, o a ordenar tus datos de manera cronológica. En esta entrada, aprenderemos cómo realizar comparaciones de fechas en Gleam.

# Cómo hacerlo

Para comparar dos fechas en Gleam, utilizaremos el módulo `gleam/chrono` que nos permite trabajar con datos temporales. Primero, importaremos el módulo en nuestro archivo:

```Gleam
import gleam/chrono
```

A continuación, crearemos dos variables con fechas para comparar:

```Gleam
let today = chrono.today()
let tomorrow = chrono.add_days(today, 1)
```

Podemos usar la función `add_days` del módulo `chrono` para obtener la fecha de mañana a partir de la fecha de hoy. Ahora, podemos usar la función `chrono.compare` para comparar estas dos fechas:

```Gleam
let comparison = chrono.compare(today, tomorrow)
```

El resultado de esta comparación será `LessThan`, `Equal` o `GreaterThan`, dependiendo de si la primera fecha es anterior, igual o posterior a la segunda fecha. Para imprimir el resultado, podemos usar una expresión `case`:

```Gleam
case comparison {
  Equal -> io.println("Las fechas son iguales")
  LessThan -> io.println("La primera fecha es anterior a la segunda")
  GreaterThan -> io.println("La primera fecha es posterior a la segunda")
}
```

La salida en consola sería:

`La primera fecha es anterior a la segunda`

# Profundizando

Si queremos comparar más que solo fechas, podemos utilizar el módulo `gleam/chrono/datetime` para trabajar con datos de fecha y hora. Este módulo nos proporciona funciones para crear fechas a partir de valores individuales de año, mes, día, hora, minuto y segundo. También podemos usar la función `from_string` para crear una fecha a partir de una cadena de texto en un formato específico.

Además, podemos cambiar el lenguaje de salida para que esté en español, por ejemplo:

```Gleam
let default_locale = chrono.Locale.default()
let spanish_locale = chrono.Locale.new(
  default_locale.language,
  default_locale.country,
  Some("es")
)
```

Esto nos permitirá obtener la salida de tiempos y fechas en español.

# Ver también

- [Documentación del módulo `gleam/chrono`](https://gleam.run/modules/chrono.html)
- [Ejemplos de comparación de fechas en Gleam](https://github.com/gleam-lang/example-code/blob/master/chrono/compare_dates.gleam)