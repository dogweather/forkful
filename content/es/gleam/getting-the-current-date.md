---
title:                "Obteniendo la fecha actual"
html_title:           "C#: Obteniendo la fecha actual"
simple_title:         "Obteniendo la fecha actual"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/gleam/getting-the-current-date.md"
---

{{< edit_this_page >}}

## ¿Qué y por qué?
Obtener la fecha actual en programación significa que se determina la fecha y hora actuales del sistema. Los programadores lo hacen para registrar eventos, hacer programación temporal y rastrear datos.

## Cómo se hace:
**Gleam,** que es de tipado estático, no tiene una función incorporada para obtener la fecha y hora actual. Usualmente, se hace a través de Erlang interoperable. Aquí tienes un ejemplo:

```Gleam
import erlang/time

pub fn main(args: List(String)) -> Nil {
  let current_time = erlang:time.now()
  |> Result.unwrap_or("Couldn't get the current time")
  |> Tuple.to_string

  io.println(current_time)
}
```
Corriendo este código, obtendrás un resultado como este:

```shell
$ gleam run main
{1593,242924,987304}
```
Estos son los segundos, microsegundos y milisegundos desde Epoch.

## Análisis en detalle
El código de ejemplo llama a la función de Erlang `time.now()`, que devuelve la hora actual en milisegundos desde Epoch. Los programadores tienen esa opción para usar Erlang debido a su eficacia probada en la operación de sistemas a gran escala.

Existen alternativas para obtener la fecha y hora actuales. En otros lenguajes, como Python o Java, hay funciones incorporadas. Además, puedes considerar la posibilidad de utilizar bibliotecas adicionales que proporcionan más funciones y mejoras.

La implementación de obtener la fecha y hora actuales puede variar dependiendo del sistema operativo y la zona horaria del sistema.

## Ver también
Para información más detallada, consulta los siguientes enlaces:

- Documentación oficial de Gleam language: https://gleam.run/docs/
- Introducción a Gleam (inglés): https://lpil.uk/blog/gleam-v020-released/
- Tiempo en Erlang (inglés):  https://erlang.org/doc/apps/erts/time_correction.html
- Tiempo y Fecha en otros lenguajes: 
    - Python (https://docs.python.org/3/library/datetime.html)
    - Java (https://docs.oracle.com/javase/8/docs/api/java/time/LocalDate.html)