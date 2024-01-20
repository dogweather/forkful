---
title:                "Analizando una fecha a partir de una cadena de texto"
html_title:           "Bash: Analizando una fecha a partir de una cadena de texto"
simple_title:         "Analizando una fecha a partir de una cadena de texto"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/gleam/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## ¿Qué y por qué?
"Parsear" una fecha desde una cadena se trata de extraer una fecha significativa de un texto sin sentido. Los programadores a menudo necesitan hacer esto para convertir datos de usuario o de archivos en un formato utilizable en sus programas.

## ¿Cómo hacerlo?

Aquí hay un simple código de ejemplo en Gleam:

```gleam
import gleam/otp/time.{StringToDatetime, StringToDatetimeFormat}

let example() {
  StringToDatetime.from_string("2023-02-03 04:14:15Z", StringToDatetimeFormat.iso8601)
}
```
En este código, la función `from_string` toma una cadena de fecha y hora y la convierte según el formato en la constante `iso8601`.

## Inmersión profunda

Tradicionalmente, en otros lenguajes de programación, obtener una fecha desde una cadena solía ser un proceso doloroso y propenso a errores. Gleam, sin embargo, hizo que esto fuera más fácil y menos propenso a errores con su módulo `gleam/otp/time`.

Existen alternativas menos seguras para analizar las fechas como el método `split` para dividir la cadena en bloques y luego construir la fecha a partir de estos bloques. Sin embargo, esta es una forma lenta no recomendada.

La implementación actual de la función `from_string` en Gleam utiliza un enfoque seguro que garantiza que la cadena esté en el formato correcto antes de convertirla en una fecha.

## Consulta también

Para más detalles sobre el manejo del tiempo en Gleam, consulta el [Manual de Gleam](https://gleam.run/manual/) y el módulo [gleam/otp/time](https://hexdocs.pm/gleam_otp/) documentación. Especialmente, aprender sobre [tipos de fecha y tiempo en Gleam](https://gleam.run/book/tour/time.html) sería beneficioso.