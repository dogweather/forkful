---
title:                "Analizando una fecha desde una cadena de texto"
html_title:           "PHP: Analizando una fecha desde una cadena de texto"
simple_title:         "Analizando una fecha desde una cadena de texto"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/ruby/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## ¿Qué y por qué?

La deconstrucción de una fecha a partir de una cadena de texto (parsing a date from a string) en programación se trata de convertir una cadena que representa una fecha en un objeto de fecha. Los programadores hacen esto para poder manipular y calcular fechas de una manera más precisa y fácil.

## ¿Cómo se hace?

Aquí te mostraré cómo hacerlo en Ruby:

```Ruby
require 'date'

str_fecha = '2022-07-30'
fecha = Date.parse(str_fecha)

puts fecha
```

Este programa tomará la cadena `str_fecha` y la convertirá en un objeto de fecha utilizando la función `Date.parse`. Si imprimimos la fecha, la salida sería:

```Ruby
2022-07-30
```

## Un vistazo más profundo

### Contexto histórico
La capacidad de convertir una cadena de texto en una fecha existe desde los primeros días de la programación, ya que es una necesidad común trabajar con fechas.

### Alternativas
Hay otras maneras de hacer esto en Ruby, usando por ejemplo Time:

```Ruby
require 'time'

str_fecha = '2022-07-30'
fecha = Time.parse(str_fecha)

puts fecha
```

### Detalles de implementación
Bajo el capó, `Date.parse` y `Time.parse` utilizan expresiones regulares y la lógica interna para desglosar la cadena en componentes de fecha y luego construir un objeto de fecha a partir de esos componentes.

## Ver también

Para más detalles, puedes consultar los siguientes enlaces:

- Método `Date.parse`: https://ruby-doc.org/stdlib-2.5.1/libdoc/date/rdoc/Date.html#method-c-parse 
- Método `Time.parse`: https://ruby-doc.org/stdlib-3.0.0/libdoc/time/rdoc/Time.html#method-c-parse