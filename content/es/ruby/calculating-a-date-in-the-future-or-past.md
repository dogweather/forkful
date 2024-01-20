---
title:                "Cálculo de una fecha en el futuro o pasado"
html_title:           "Ruby: Cálculo de una fecha en el futuro o pasado"
simple_title:         "Cálculo de una fecha en el futuro o pasado"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/ruby/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

# Calcular una Fecha en el Futuro o Pasado en Ruby

## ¿Qué y Por Qué?

Calcular una fecha en el futuro o pasado es una tarea común en programación. Esto se hace para realizar eventos de seguimiento, recordatorios y otras funciones que dependen del tiempo. 

## Cómo Hacerlo:

En Ruby, es bastante fácil calcular una fecha en el futuro o pasado. Aquí te muestro cómo:

```Ruby
require 'date'

hoy = Date.today
puts "Hoy es: #{hoy}"

# Una semana en el futuro
futuro = hoy + 7
puts "Dentro de una semana será: #{futuro}"

# Una semana en el pasado
pasado = hoy - 7
puts "Hace una semana fue: #{pasado}"
```

Estos son los resultados que obtendrás:

```Ruby
Hoy es: 2022-01-10
Dentro de una semana será: 2022-01-17
Hace una semana fue: 2022-01-03
```

## Profundización

El cálculo de fechas ha sido parte de la programación desde sus inicios, necesarios para tareas como la planificación de tareas y la gestión de base de datos. Ruby simplifica este proceso con la biblioteca Date. 

Existen alternativas para calcular fechas en Ruby, como la gema Time, que también maneja zonas horarias. Sin embargo, Date es suficiente para la mayoría de las situaciones.

Ruby realiza este cálculo internamente sumando o restando un número determinado de días a la fecha actual. Recuerda que Ruby considera que un año tiene 365.2425 días al realizar estos cálculos.

## Ver También

Aquí tienes algunos enlaces que podrían ser de tu interés:

1. Documentación oficial de Ruby sobre la biblioteca Date: [Ruby Doc - Date](https://ruby-doc.org/stdlib-2.7.0/libdoc/date/rdoc/Date.html)

2. Tutorial rápido sobre el tiempo y las fechas en Ruby: [Dates and Times in Ruby](http://www.tutorialspoint.com/ruby/ruby_date_time.htm)