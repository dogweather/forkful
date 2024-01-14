---
title:    "Gleam: Convirtiendo una fecha en una cadena"
keywords: ["Gleam"]
---

{{< edit_this_page >}}

## Por Qué

Convertir una fecha en una cadena de texto es una tarea común en la programación, ya sea para mostrar la fecha en un formato específico o para realizar operaciones con ella. En Gleam, esta conversión se puede realizar de manera sencilla y eficiente gracias a sus características de tipado estático y manipulación de cadenas de texto.

## Cómo Hacerlo

Para convertir una fecha en una cadena de texto en Gleam, necesitamos primero tener una fecha en formato `DateTime`. Esto se puede hacer utilizando la función `DateTime.fromString` y pasándole la fecha en formato ISO 8601.

```
Gleam> import gleam/datetime.{DateTime}

Gleam> my_date = DateTime.fromString("2021-01-01T00:00:00.000000Z")
```

Una vez que tenemos nuestra fecha en formato `DateTime`, podemos utilizar la función `DateTime.toString` para convertirla en una cadena de texto en el formato deseado. Por ejemplo, si queremos mostrar la fecha en formato DD/MM/YYYY, podemos hacer lo siguiente:

```
Gleam> my_date |> DateTime.toString("%d/%m/%Y")
=> "01/01/2021"
```

## Profundizando

La función `DateTime.toString` puede aceptar diferentes formatos de cadena, lo que nos permite personalizar la salida de fecha según nuestras necesidades. Además, la biblioteca estándar de Gleam ofrece muchas más funciones relacionadas con fechas y tiempos, como `DateTime.add`, `DateTime.diff` y `DateTime.month`.

En el caso de necesitar manejar zonas horarias, Gleam también cuenta con el módulo `Chrono`, que nos permite manipular fechas y tiempos en diferentes zonas horarias.

## Ver También

- Documentación oficial de Gleam sobre fechas y tiempos: https://gleam.run/modules/datetime.html
- Funciones relacionadas con fechas y tiempos en la biblioteca estándar de Gleam: https://gleam.run/modules/datetime.html#functions
- Módulo `Chrono` para manejar zonas horarias: https://gleam.run/modules/chrono.html