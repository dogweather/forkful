---
title:                "Obteniendo la fecha actual"
date:                  2024-01-20T15:16:23.525311-07:00
html_title:           "Bash: Obteniendo la fecha actual"
simple_title:         "Obteniendo la fecha actual"

tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/ruby/getting-the-current-date.md"
---

{{< edit_this_page >}}

## What & Why?
Obtener la fecha actual es coger la fecha de hoy desde nuestro código. Los programadores usamos esta función para tareas como guardar un timestamp, marcar eventos, o simplemente mostrar la fecha en una aplicación.

## How to:
En Ruby, puedes obtener la fecha actual de manera sencilla usando la clase `Date` o `Time`. Aquí te dejo unos ejemplos:

```Ruby
require 'date'

# Obtener la fecha actual con Date
fecha_actual_date = Date.today
puts fecha_actual_date
# => 2023-04-14 (esto variará según el día en que ejecutes el código)

# Obtener la fecha y hora actual con Time
fecha_actual_time = Time.now
puts fecha_actual_time
# => 2023-04-14 21:03:39 +0200 (esto también variará)
```

## Deep Dive:
En Ruby, tradicionalmente se ha usado la clase `Time` para manejar fecha y hora. Pero `Time` tiene sus limitaciones, especialmente en cuanto a la representación de fechas históricas o futuras lejanas. Ahí es donde entra en juego la gema `date`, que fue estandarizada en Ruby 1.9.

La gema `date` trae la clase `Date`, que se enfoca en fechas (sin hora), y `DateTime`, para cuando necesitás más precisión incluyendo la hora. Estas clases pueden manejar un rango de fechas mucho más amplio: `Date` desde el año -4712 hasta el año infinito, y `DateTime` con precisiones de segundo o fracción de segundo.

Un detalle curioso es que `Time` en versiones antiguas de Ruby estaba limitada al rango de tiempo del sistema operativo. Pero con Ruby 1.9.2 en adelante, `Time` utiliza una representación interna que no depende del sistema, por lo que también puede representar un rango de fechas tan amplio como `DateTime`.

En cuando a la zona horaria, `Time` maneja zonas horarias y `Date` y `DateTime` no. Sin embargo, puedes usar la gema `tzinfo` para trabajar con zonas horarias si usas `Date` o `DateTime`.

Alternativas como la gema `active_support` (parte de Rails) también ofrecen extensiones para manejar fechas, pero para uso estándar en Ruby puro, `Date` y `Time` son más que suficientes.

## See Also:
- Documentación oficial de Ruby para la clase `Time`: [https://ruby-doc.org/core-2.7.0/Time.html](https://ruby-doc.org/core-2.7.0/Time.html)
- Documentación oficial de Ruby para la clase `Date`: [https://ruby-doc.org/stdlib-2.7.0/libdoc/date/rdoc/Date.html](https://ruby-doc.org/stdlib-2.7.0/libdoc/date/rdoc/Date.html)
- Guía sobre gestión de zonas horarias con `tzinfo`: [https://www.rubydoc.info/gems/tzinfo/frames](https://www.rubydoc.info/gems/tzinfo/frames)
- Gema `active_support` y sus extensiones para manejo de tiempo: [https://guides.rubyonrails.org/active_support_core_extensions.html#extensions-to-time](https://guides.rubyonrails.org/active_support_core_extensions.html#extensions-to-time)
