---
title:                "Ruby: Calculando una fecha en el futuro o pasado"
programming_language: "Ruby"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/ruby/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Por qué

Calcular fechas en el futuro o en el pasado es una tarea común en la programación, especialmente cuando se trabaja con datos relacionados con el tiempo. Puede ser útil para predecir eventos futuros o realizar cálculos de duración.

## Cómo hacerlo

Para calcular una fecha en el futuro o en el pasado en Ruby, se puede utilizar el método `+` o `-` junto con un objeto de clase `Date` o `Time`. Por ejemplo:

``` Ruby
fecha_actual = Date.today
# => #<Date: 2021-08-28 ((2459461j,0s,0n),+0s,2299161j)>
fecha_futura = fecha_actual + 7
# => #<Date: 2021-09-04 ((2459468j,0s,0n),+0s,2299161j)>
fecha_pasada = fecha_actual - 7
# => #<Date: 2021-08-21 ((2459454j,0s,0n),+0s,2299161j)>
```

En este ejemplo, utilizamos el método `Date.today` para obtener la fecha actual y luego creamos una nueva fecha en el futuro sumando 7 días a la fecha actual con el método `+`. De manera similar, podemos obtener una fecha en el pasado restando días con el método `-`. 

También es posible especificar una fecha exacta utilizando el método `Date.new`, pasando los parámetros para año, mes y día. Por ejemplo:

``` Ruby
nueva_fecha = Date.new(2021, 9, 5)
# => #<Date: 2021-09-05 ((2459475j,0s,0n),+0s,2299161j)>
```

Una vez que tengamos una fecha específica, podemos utilizar los mismos métodos `+` y `-` para calcular fechas en el futuro o en el pasado con respecto a esa fecha.

## Profundizando

Además de utilizar el método `+` y `-`, Ruby también proporciona otros métodos útiles para calcular fechas en el futuro o en el pasado. Algunos de ellos son:

- `next_day`: devuelve la fecha del día siguiente
- `prev_day`: devuelve la fecha del día anterior
- `next_month`: devuelve la fecha del mes siguiente
- `prev_month`: devuelve la fecha del mes anterior
- `next_year`: devuelve la fecha del año siguiente
- `prev_year`: devuelve la fecha del año anterior

Por ejemplo:

``` Ruby
fecha_actual = Date.today
# => #<Date: 2021-08-28 ((2459461j,0s,0n),+0s,2299161j)>
fecha_siguiente_mes = fecha_actual.next_month
# => #<Date: 2021-09-28 ((2459491j,0s,0n),+0s,2299161j)>
fecha_anterior_año = fecha_actual.prev_year
# => #<Date: 2020-08-28 ((2459074j,0s,0n),+0s,2299161j)>
```

También es posible realizar cálculos de duración utilizando el método `difference` entre dos fechas. Este método devuelve la diferencia en días entre las dos fechas. Por ejemplo:

``` Ruby
fecha_inicio = Date.new(2021, 8, 20)
fecha_final = Date.new(2021, 8, 30)
duracion = fecha_final - fecha_inicio
# => 10
```

## Ver también

- Documentación oficial de Ruby sobre fechas y tiempo: https://ruby-doc.org/stdlib-2.7.0/libdoc/date/rdoc/index.html
- Un tutorial sobre cálculos de fechas en Ruby: https://www.rubyguides.com/2018/08/ruby-date-and-time/
- Una guía detallada sobre el manejo de fechas y tiempo en Ruby: https://blog.appsignal.com/2020/06/24/ruby-magic-date-time-manipulation.html