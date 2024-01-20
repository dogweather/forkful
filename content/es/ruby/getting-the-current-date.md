---
title:                "Obteniendo la fecha actual"
html_title:           "C#: Obteniendo la fecha actual"
simple_title:         "Obteniendo la fecha actual"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/ruby/getting-the-current-date.md"
---

{{< edit_this_page >}}

# Trabajando con fechas en Ruby: Cómo obtener la fecha actual

## ¿Qué y Por qué?

Obtener la fecha actual en un programa es una forma de capturar el estado temporal preciso al momento de ejecución. Es esencial cuando manejamos estadísticas, trazabilidad en registros(logs), posts en un blog, entre otros.

## Cómo hacerlo: 
```Ruby
require 'date'
puts Date.today
``` 
Esto imprimirá la fecha en formato 'yyyy-mm-dd'. Por ejemplo, si hoy es 22 de febrero de 2023, la salida será `2023-02-22`.

Si deseas agregar la hora, solo usa `DateTime` en lugar de `Date`:
```Ruby
require 'date'
puts DateTime.now
``` 
Esto generará una salida parecida a `2023-02-22T11:20:30+00:00`, que incluye la fecha, la hora, los minutos, los segundos y el huso horario.

## Buceo Profundo

Historia: Ruby ha incluido el módulo de `Date` desde su versión 1.9 para facilitar el trabajo con fechas. Antes de eso, los programadores tenían que recurrir a la biblioteca estándar de `Time`, que no tiene tantas funciones relacionadas con las fechas.

Alternativas: Aunque `Date` y `DateTime` son suficientes para la mayoría de los casos, también puedes usar `Time` para obtener una marca de tiempo de Unix (segundos desde la época 1970-01-01 00:00:00 UTC). Aquí tienes un ejemplo:
```Ruby
puts Time.now.to_i
``` 

Un detalle de implementación a tener en cuenta es la precisión. `DateTime.now` sólo tiene precisión hasta el segundo, mientras que `Time.now` puede tener una precisión de hasta un nanosegundo.

## Ver También

- La documentación oficial de `Date`: [https://ruby-doc.org/stdlib-3.0.0/libdoc/date/rdoc/Date.html](https://ruby-doc.org/stdlib-3.0.0/libdoc/date/rdoc/Date.html)
- La documentación oficial de `DateTime`: [https://ruby-doc.org/stdlib-3.0.0/libdoc/date/rdoc/DateTime.html](https://ruby-doc.org/stdlib-3.0.0/libdoc/date/rdoc/DateTime.html)