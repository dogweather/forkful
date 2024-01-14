---
title:                "Ruby: Calculando una fecha en el futuro o pasado"
simple_title:         "Calculando una fecha en el futuro o pasado"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/ruby/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

# ¿Por qué?

Calcular fechas en el futuro o pasado es una tarea común en la programación. Puede ser necesario para realizar tareas como programar una reunión, programar pagos o mostrar un calendario en una aplicación.

# ¿Cómo hacerlo?

Para calcular una fecha en el futuro o pasado, podemos usar la clase de Ruby Date. Primero, debemos requerir la clase escribiendo `require 'date'` en nuestro código. Luego, podemos utilizar el método `today` para obtener la fecha actual en un objeto Date. Después, podemos usar el método `+` o `-` seguido de un número entero para agregar o restar días al objeto Date. Por ejemplo:

```
require 'date'
hoy = Date.today
mañana = hoy + 1
ayer = hoy - 1
```

Podemos imprimir estas fechas para ver el resultado:

```
puts hoy
puts mañana
puts ayer
```

La salida debería ser algo como:

```
# => 2020-05-05
# => 2020-05-06
# => 2020-05-04
```

# Buceo profundo

La clase Date de Ruby tiene muchos más métodos que podemos utilizar para calcular fechas en el futuro o pasado. Por ejemplo, podemos usar los métodos `next_day` y `prev_day` para obtener la fecha del día siguiente o anterior a una fecha determinada. También podemos utilizar el método `beginning_of_week` para obtener el primer día de la semana de una fecha dada.

Además, también podemos especificar una fecha específica en vez de usar la fecha actual. Podemos hacer esto utilizando el método `strptime` y pasándole una cadena de fecha en el formato deseado. Por ejemplo:

```
fecha = Date.strptime("2020-05-25", "%Y-%m-%d")
```

Esto creará un objeto Date con la fecha especificada. Luego podemos utilizar los métodos mencionados anteriormente para calcular fechas en el futuro o pasado basadas en esta fecha.

# Ver también

- [Documentación de Ruby sobre la clase Date](https://ruby-doc.org/stdlib-2.7.1/libdoc/date/rdoc/Date.html)
- [Video explicativo sobre cómo calcular fechas en Ruby](https://www.youtube.com/watch?v=JZ1dHGxAOZU)
- [Ejemplo de código sobre cómo calcular una fecha en el pasado en Ruby](https://www.rubyguides.com/2015/10/ruby-date-format-in-12-minutes/)

Recuerda siempre revisar la documentación y experimentar con diferentes métodos para encontrar el mejor enfoque para tu proyecto específico. ¡Feliz codificación!