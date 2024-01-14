---
title:    "Javascript: Calculando una fecha en el futuro o pasado"
keywords: ["Javascript"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/es/javascript/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## ¿Por qué calcular una fecha en el futuro o pasado?

La programación con Javascript puede ser útil para muchos propósitos, y calcular una fecha en el futuro o pasado es una de esas tareas. Ya sea para planificar eventos, realizar tareas de seguimiento o crear recordatorios, tener la habilidad de calcular fechas puede ser de gran ayuda.

## Cómo hacerlo

Para calcular una fecha en el futuro o pasado utilizando Javascript, podemos utilizar el objeto `Date`. Por ejemplo, si queremos obtener la fecha de mañana, podemos hacer lo siguiente:

```Javascript
let mañana = new Date();
mañana.setDate(mañana.getDate() + 1);
```

De manera similar, si queremos obtener la fecha de ayer, podemos hacer lo siguiente:

```Javascript
let ayer = new Date();
ayer.setDate(ayer.getDate() - 1);
```

También podemos utilizar la función `getFullYear()`, `getMonth()`, `getDate()` para obtener el año, mes y día respectivamente de una fecha determinada y luego modificarlos de acuerdo a nuestros cálculos. Por ejemplo, si queremos obtener la fecha dentro de una semana, podemos hacer lo siguiente:

```Javascript
let fecha = new Date();
fecha.setDate(fecha.getDate() + 7);
console.log('La fecha dentro de una semana es: ' + fecha);
```

Este código nos mostrará la fecha dentro de una semana en el formato de `MM/DD/YYYY`. Podemos adaptar estos ejemplos para calcular cualquier fecha en el futuro o pasado según nuestras necesidades.

## Inmersión profunda

Si queremos calcular fechas en el futuro o pasado de manera más precisa, podemos utilizar la librería `moment-js` que nos permite trabajar con fechas de forma más sencilla y flexible. Esta librería nos permite realizar operaciones con fechas, como agregar o restar días, meses o años, comparar fechas, obtener el último día de un mes, entre otros.

Para utilizar `moment-js`, primero debemos instalarlo en nuestro proyecto a través de la línea de comandos con `npm install moment` y luego importarlo en nuestro archivo Javascript:

```Javascript
const moment = require('moment');
```

Por ejemplo, si queremos obtener la fecha de dentro de un mes a partir de hoy, podemos utilizar la función `add()` y especificar el intervalo de tiempo que queremos agregar:

```Javascript
let fecha = moment().add(1, 'month');
console.log('La fecha dentro de un mes es: ' + fecha.format('MM/DD/YYYY'));
```

En este caso, especificamos que queremos agregar 1 mes. También podemos especificar otros intervalos de tiempo como días, años, horas, minutos, entre otros.

## Ver también

- [Objeto Date en Javascript](https://developer.mozilla.org/es/docs/Web/JavaScript/Reference/Global_Objects/Date)
- [Moment.js](https://momentjs.com/)
- [Cómo trabajar con fechas en Javascript](https://www.digitalocean.com/community/tutorials/how-to-work-with-dates-in-javascript)