---
title:                "Javascript: Calculando una fecha en el futuro o pasado"
simple_title:         "Calculando una fecha en el futuro o pasado"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/javascript/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Por qué

El cálculo de fechas en el futuro o pasado es una habilidad útil en la programación ya que permite realizar tareas como programar eventos o crear recordatorios. En Javascript, esta tarea se puede realizar de manera sencilla utilizando ciertas funciones y métodos.

## Cómo hacerlo

Para calcular una fecha en el futuro o pasado, primero debemos definir la fecha base de la cual queremos partir. En este caso, utilizaremos la función incorporada `new Date()`, que crea una instancia de un objeto con la fecha actual. A continuación, podemos utilizar los métodos `setFullYear()` para establecer el año, `setMonth()` para el mes y `setDate()` para el día en la fecha base. 

Por ejemplo, si queremos calcular la fecha de un mes después a partir de hoy, podemos usar el método `setMonth()` para agregar 1 al mes actual:

```Javascript
var hoy = new Date(); //Fecha actual
hoy.setMonth(hoy.getMonth() + 1); //Sumamos 1 al mes actual
console.log(hoy); // Output: Wed Sep 29 2021
```

De manera similar, si queremos calcular la fecha de un año atrás a partir de hoy, podemos usar el método `setFullYear()` para restar 1 al año actual:

```Javascript
var hoy = new Date(); //Fecha actual
hoy.setFullYear(hoy.getFullYear() - 1); //Restamos 1 al año actual
console.log(hoy); // Output: Mon Aug 30 2020
```

## Profundizando

Además de los métodos mencionados, también podemos utilizar `setDate()` para cambiar el día en una fecha, `setHours()` para cambiar las horas y `setMinutes()` para cambiar los minutos. Estos métodos pueden ser combinados para obtener una fecha precisa en el futuro o pasado.

También es importante mencionar la función `getTime()`, que devuelve el tiempo en milisegundos desde enero de 1970. Esto puede ser útil para realizar cálculos de fechas en milisegundos y luego convertirlos a una fecha legible.

Por último, es importante tener en cuenta que los valores pasados a estos métodos deben ser números enteros. Si se pasan valores decimales, se redondearán al número entero más cercano.

## Ver también

- [Documentación de Date en MDN](https://developer.mozilla.org/es/docs/Web/JavaScript/Reference/Global_Objects/Date)
- [Tutoriales de Javascript en Platzi](https://platzi.com/tutoriales/javascript/)
- [Date en Javascript: Cómo manipular fechas y horas](https://www.adictosaltrabajo.com/2012/09/14/date-en-javascript-como-manipular-fechas-y-horas/)