---
title:                "Ruby: Comparando dos fechas"
programming_language: "Ruby"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/ruby/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Por qué
Si bien puede parecer una tarea simple, comparar dos fechas en Ruby es una habilidad útil en la programación. Puede ser útil para verificar si una fecha está dentro de un cierto rango o para ordenar una lista de fechas. Aprender cómo comparar fechas en Ruby puede ahorrar tiempo y mejorar tu código.

## Cómo hacerlo
Para comparar dos fechas en Ruby, utilizaremos el método `.to_date`, que convierte un objeto de fecha y hora en un objeto de fecha. Luego, podemos usar los operadores de comparación como `<`, `>` y `==` para comparar las dos fechas. Por ejemplo, si queremos verificar si una fecha está antes de otra, podemos usar el siguiente código:

```Ruby
fecha1 = "01/01/2020"
fecha2 = "01/01/2021"

if fecha1.to_date < fecha2.to_date
  puts "La fecha 1 es anterior a la fecha 2"
end
```

La salida en la consola sería: `La fecha 1 es anterior a la fecha 2`.

También podemos comparar fechas usando el método `.compare_to` que devuelve un número positivo si la fecha es más reciente, un número negativo si es más temprana y 0 si son iguales. Aquí está un ejemplo que muestra esto en acción:

```Ruby
fecha1 = "01/01/2020"
fecha2 = "01/01/2021"

comparacion = fecha1.compare_to(fecha2)

if comparacion > 0
  puts "La fecha 1 es más reciente que la fecha 2"
elsif comparacion < 0
  puts "La fecha 1 es más temprana que la fecha 2"
else
  puts "Ambas fechas son iguales"
end
```

La salida sería: `La fecha 1 es más temprana que la fecha 2`.

## Profundizando
Puede haber casos en los que solo queremos comparar ciertos aspectos de una fecha, como el mes o el año. Para eso, podemos usar el método `.strftime` para formatear la fecha en una cadena y luego comparar esas cadenas. Por ejemplo, si solo queremos comparar el mes y el año, podemos hacerlo de la siguiente manera:

```Ruby
fecha1 = "01/01/2020"
fecha2 = "01/01/2021"

if fecha1.strftime("%m-%Y") == fecha2.strftime("%m-%Y")
  puts "Ambas fechas tienen el mismo mes y año"
end
```

La salida sería: `Ambas fechas tienen el mismo mes y año`.

Otra cosa a tener en cuenta al comparar fechas es que si se utiliza el operador `==` en objetos de fecha y hora, devolverá `false` incluso si representan la misma fecha y hora exactos. Para evitar esto, podemos utilizar el método `.to_i` que devuelve un número entero que representa el número de segundos desde la época de Unix (1 de enero de 1970). Luego, podemos comparar estos números enteros para determinar si las fechas son iguales.

¡Y eso es todo! Ahora sabes cómo comparar fechas en Ruby y tener más control sobre tu código.

## Ver también
- [Documentación de Ruby: DateTime](https://ruby-doc.org/stdlib-2.7.1/libdoc/date/rdoc/DateTime.html)
- [Comparación de fechas en Ruby: explicada](https://medium.com/@kyleeecodes/ruby-date-comparison-explained-fb9428858e6d)
- [Diferencia entre dos fechas en Ruby](https://gist.github.com/jeremyw/993506)

¡Feliz programación! 🚀