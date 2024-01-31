---
title:                "Análisis de una fecha a partir de una cadena"
date:                  2024-01-20T15:38:06.808605-07:00
html_title:           "Arduino: Análisis de una fecha a partir de una cadena"
simple_title:         "Análisis de una fecha a partir de una cadena"

tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/ruby/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## Qué y Por Qué?
Convertir una fecha de texto a formato de fecha permite manipularla con código. Los programadores lo hacen para validar, almacenar o cambiar formatos de fechas de manera confiable.

## Cómo hacerlo:
```Ruby
require 'date'

# Convertir una cadena de texto a un objeto DateTime
fecha_texto = '2022-03-15'
fecha_objeto = Date.parse(fecha_texto)
puts fecha_objeto   # => 2022-03-15

# Cambio de formato de fecha
puts fecha_objeto.strftime('%d/%m/%Y') # => 15/03/2022

# Añadir días a la fecha
nueva_fecha = fecha_objeto + 10
puts nueva_fecha   # => 2022-03-25
```

## Inmersión Profunda
En los inicios de Ruby, la conversión de fechas requería bibliotecas externas, pero ahora DateTime y Time son parte del estándar. `Date.parse` puede fallar si el formato es inesperado; aquí es mejor `Date.strptime` donde defines el formato. Alternativamente, podrías usar la gema 'Chronic' para un análisis más natural del lenguaje.

## Vea También
- Documentación de Ruby para `Date`: [ruby-doc.org/stdlib-3.0.0/libdoc/date/rdoc/Date.html](https://ruby-doc.org/stdlib-3.0.0/libdoc/date/rdoc/Date.html)
- Ayuda de strftime en español: [apidock.com/ruby/DateTime/strftime](https://apidock.com/ruby/DateTime/strftime)
- Gema 'Chronic' para análisis de fechas: [github.com/mojombo/chronic](https://github.com/mojombo/chronic)
