---
title:                "Calculando una fecha en el futuro o pasado"
html_title:           "Ruby: Calculando una fecha en el futuro o pasado"
simple_title:         "Calculando una fecha en el futuro o pasado"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/ruby/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

¿Por qué calcular una fecha en el futuro o en el pasado? Es importante poder predecir eventos o planificar tareas en fechas específicas, y conocer cómo hacerlo en Ruby puede facilitar este proceso.

## ¿Cómo calcular una fecha en el futuro o en el pasado?

Es bastante sencillo utilizar la función `DateTime` en Ruby para calcular fechas en el futuro o en el pasado. Primero, debemos importar la librería `date` utilizando `require 'date'`. Luego, podemos utilizar el método `DateTime.now` para obtener la fecha y hora actuales. A partir de ahí, podemos añadir o restar días, meses o años utilizando los métodos `next_day`, `prev_day`, `next_month` o `prev_month` según sea necesario. Por último, podemos utilizar el método `strftime` para darle formato a nuestra fecha de salida.

```ruby
require 'date'

today = DateTime.now
future_date = today.next_year(2).next_month(3).next_day(10)
puts "La fecha en 2 años, 3 meses y 10 días será #{future_date.strftime("%d/%m/%Y")}"
# Salida: La fecha en 2 años, 3 meses y 10 días será 10/08/2022
```

También podemos utilizar fechas específicas para realizar cálculos. Por ejemplo, si queremos conocer la fecha de dentro de 2 semanas, podemos utilizar el método `parse` para introducir una fecha específica y luego utilizar `next_day` para añadir los días necesarios.

```ruby
require 'date'

specific_date = DateTime.parse("2020/10/01")
new_date = specific_date.next_day(14)
puts "La fecha de dentro de dos semanas será #{new_date.strftime("%d/%m/%Y")}"
# Salida: La fecha de dentro de dos semanas será 15/10/2020
```

## Profundizando en el cálculo de fechas en el futuro o en el pasado

La clase `DateTime` en Ruby ofrece una gran variedad de métodos para manipular fechas. Además de los mencionados anteriormente, también podemos utilizar `next_year` y `prev_year` para añadir o restar años, `next_hour` y `prev_hour` para añadir o restar horas, entre otros. Además, podemos utilizar el método `new` para crear una fecha específica a partir de parámetros dados.

```ruby
require 'date'

specific_date = DateTime.new(year, month, day, hour, minute, second)
```

En resumen, calcular fechas en el futuro o en el pasado en Ruby puede ser muy útil y sencillo utilizando la clase `DateTime` y sus métodos. Con un poco de práctica, podremos planificar cualquier tarea o evento sin problemas.

## Ver también

- Documentación oficial de `DateTime` en Ruby: https://ruby-doc.org/stdlib-2.7.1/libdoc/date/rdoc/DateTime.html
- Tutorial sobre manipulación de fechas en Ruby: https://www.rubyguides.com/2015/05/ruby-date-format/
- Ejemplos de código para cálculo de fechas en diferentes lenguajes: https://www.includehelp.com/code-snippets/calculate-date-of-future-past.aspx