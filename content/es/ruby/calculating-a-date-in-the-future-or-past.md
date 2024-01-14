---
title:    "Ruby: Calculando una fecha en el futuro o pasado"
keywords: ["Ruby"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/es/ruby/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Porqué

Calcular fechas en el futuro o en el pasado es una tarea común en la programación. Desde la planificación de eventos hasta la gestión de citas, conocer cómo calcular una fecha específica puede ser muy útil en muchas aplicaciones.

## Cómo hacerlo

En Ruby, hay varias formas de calcular una fecha en el futuro o en el pasado. Para empezar, podemos utilizar el método `Time.new` para crear una fecha y luego utilizar los métodos `+` o `-` para añadir o restar días, meses o años.

Por ejemplo, para calcular una fecha 10 días en el futuro, podemos utilizar el siguiente código:

```Ruby
future_date = Time.new + (10 * 24 * 60 * 60)
puts future_date
```

Este código creará una nueva instancia de `Time` para la fecha y hora actual, y luego añadirá 10 días (10*24 horas * 60 minutos * 60 segundos) a esa fecha. Al imprimir `future_date`, deberíamos obtener la fecha exacta 10 días en el futuro.

También podemos utilizar el método `strftime` para dar formato a la fecha de salida. Por ejemplo, si queremos imprimir la fecha en formato "DD/MM/YYYY", podemos hacerlo de la siguiente manera:

```Ruby
puts future_date.strftime("%d/%m/%Y")
```

Este método toma una cadena de formato como argumento y devuelve la fecha en el formato indicado. En este caso, "%d" representa el día, "%m" representa el mes y "%Y" representa el año en formato de cuatro dígitos.

## Profundizando

En Ruby, el cálculo de fechas se basa en el número de segundos desde el 1 de enero de 1970, también conocido como la "época UNIX". Por lo tanto, al sumar o restar días, meses o años a una fecha, en realidad estamos añadiendo o restando un número determinado de segundos.

También es importante tener en cuenta que las fechas son objetos inmutables en Ruby, lo que significa que al manipular las fechas, en realidad estamos creando nuevas instancias en lugar de cambiar la fecha original.

Otra forma útil de calcular fechas en Ruby es utilizando la gema `date`. Esta gema nos proporciona una clase `Date` con métodos más intuitivos para realizar cálculos de fecha. Por ejemplo, en lugar de utilizar el método `+` para sumar días, podemos utilizar el método `next_day` para obtener la fecha siguiente.

## Ver también

- [Documentación oficial de Ruby sobre Time](https://ruby-doc.org/core-2.7.1/Time.html)
- [Cálculos básicos de fechas en Ruby](https://www.rubyguides.com/2015/05/working-with-dates-ruby/)
- [Calculando fechas con la gema Date en Ruby](https://medium.com/@alexvpopov/calculate-time-in-the-future-with-ruby-d32413fb4ab1)