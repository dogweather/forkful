---
title:    "Ruby: Calculando una fecha en el futuro o pasado."
keywords: ["Ruby"]
---

{{< edit_this_page >}}

## Por qué

Calcular la fecha en el futuro o en el pasado puede ser una tarea útil en programación, ya sea para programar eventos o para realizar cálculos de tiempo específicos. En Ruby, hay varias formas de lograr esto, y en este artículo te explicaremos cómo hacerlo.

## Cómo hacerlo

Hay dos formas principales de calcular una fecha en el futuro o en el pasado en Ruby: utilizando la clase `Date` o la clase `Time`. Ambas ofrecen métodos y funcionalidades útiles para trabajar con fechas y tiempos.

### Utilizando la clase `Date`

Para calcular una fecha en el futuro o en el pasado utilizando la clase `Date`, primero necesitamos crear una instancia de esa clase con la fecha actual. Esto se puede hacer llamando al método `today`, de la siguiente manera:

```Ruby
today = Date.today
```

A continuación, podemos utilizar el método `+` para sumar o restar días a esa fecha. Por ejemplo, si queremos calcular la fecha 7 días en el futuro, podemos hacer lo siguiente:

```Ruby
future_date = today + 7
```

De manera similar, si queremos calcular la fecha 3 días en el pasado, utilizamos el método `-`:

```Ruby
past_date = today - 3
```

El resultado de estas operaciones será una nueva instancia de la clase `Date` con la fecha deseada. Podemos imprimir esta fecha en un formato legible utilizando el método `strftime`, como se muestra a continuación:

```Ruby
puts future_date.strftime('%d/%m/%Y') # Imprime "25/07/2020"
puts past_date.strftime('%d/%m/%Y') # Imprime "15/07/2020"
```

### Utilizando la clase `Time`

Otra forma de calcular una fecha en el futuro o en el pasado es utilizando la clase `Time`. La diferencia con la clase `Date` es que también podemos trabajar con la hora en este caso.

Al igual que con la clase `Date`, primero creamos una instancia de la clase `Time` con la fecha y hora actuales. Luego, podemos utilizar el método `+` y `-` para sumar o restar segundos, minutos, horas, días, etc.

Veamos un ejemplo:

```Ruby
now = Time.now
future_time = now + 60 # Sumamos 60 segundos a la fecha y hora actual
past_time = now - 3600 # Restamos 3600 segundos (1 hora) a la fecha y hora actual
```

Igual que antes, podemos imprimir estas fechas y horas en un formato legible utilizando el método `strftime`:

```Ruby
puts future_time.strftime('%H:%M:%S') # Imprime "20:36:00"
puts past_time.strftime('%H:%M:%S') # Imprime "18:23:00"
```

## Profundizando

Para profundizar aún más en el cálculo de fechas en Ruby, es importante conocer los diferentes formatos de fechas y horas que se pueden utilizar en los métodos `strftime` y `parse`. Además, se pueden utilizar otros métodos y funcionalidades de las clases `Date` y `Time` para trabajar con fechas y horas de forma más compleja.

## Ver también

- [Documentación de la clase `Date` en Ruby](https://ruby-doc.org/stdlib-2.7.1/libdoc/date/rdoc/Date.html)
- [Documentación de la clase `Time` en Ruby](https://ruby-doc.org/core-2.7.1/Time.html)
- [Métodos `strftime` y `parse` en Ruby](https://ruby-doc.org/core-2.7.1/Time.html#method-i-strftime)
- [Formateo de fechas en Ruby](https://devhints.io/datetime)
- [Ejemplos de cálculo de fechas en Ruby](https://www.rubyguides.com/2015/10/ruby-datetime/)