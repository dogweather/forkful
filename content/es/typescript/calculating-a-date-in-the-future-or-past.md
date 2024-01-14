---
title:    "TypeScript: Calculando una fecha en el futuro o en el pasado"
keywords: ["TypeScript"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/es/typescript/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

# Por qué calcular una fecha en el futuro o pasado en TypeScript

Muchas veces, al trabajar en proyectos de programación, nos encontramos con la necesidad de calcular una fecha en el futuro o pasado. Esto puede ser útil para implementar funcionalidades relacionadas con fechas en nuestras aplicaciones o para realizar operaciones con fechas en cálculos matemáticos. En este artículo, vamos a ver cómo hacer esto utilizando TypeScript.

## Cómo hacerlo
Para calcular una fecha en el futuro o pasado en TypeScript, primero necesitaremos importar la librería `Date` de JavaScript. Luego, vamos a crear una nueva instancia de esta clase con la fecha actual y utilizaremos sus métodos `setDate()` y `getDate()` para realizar los cálculos necesarios. Veamos un ejemplo de esto:

```TypeScript
// Importamos la librería Date
import { Date } from 'Date';

// Creamos una instancia con la fecha actual
let fechaActual = new Date();

// Sumamos 7 días a la fecha actual
fechaActual.setDate(fechaActual.getDate() + 7);
console.log(fechaActual); // Output: 2021-09-09T03:00:00.000Z
```

En este ejemplo, hemos sumado 7 días a la fecha actual y utilizando el método `getDate()` para obtener el día del mes en formato numérico. Si queremos restar días o incluso meses o años, podemos utilizar los métodos `setDate()`, `setMonth()` y `setFullYear()` respectivamente. Ahora, veamos un ejemplo de cómo calcular una fecha en el pasado:

```TypeScript
// Restamos 1 mes a la fecha actual
fechaActual.setMonth(fechaActual.getMonth() - 1);
console.log(fechaActual); // Output: 2021-07-09T03:00:00.000Z
```

Como podemos ver, utilizando los métodos mencionados podemos realizar cálculos con fechas en el futuro o pasado de una manera sencilla.

## Profundizando en el cálculo de fechas
Para profundizar en el tema, es importante tener en cuenta algunos aspectos específicos de la clase `Date` en TypeScript. Por ejemplo, el mes en esta clase comienza en 0, por lo que para sumar o restar meses tendremos que tenerlo en cuenta y sumar o restar 1 al valor que queramos modificar. Además, la clase `Date` también tiene métodos para obtener el día de la semana en formato numérico (0 para domingo, 1 para lunes, etc.) y para obtener la hora, minutos y segundos de una fecha.

Otra opción para calcular fechas en el futuro o pasado es utilizar librerías externas como Moment.js, que nos brindan una variedad de métodos y funciones para trabajar con fechas de manera más fácil y precisa. Sin embargo, la clase `Date` de JavaScript y los métodos mencionados anteriormente suelen ser suficientes para la mayoría de los casos.

## Ver también
- [Documentación oficial de la clase `Date` en TypeScript](https://developer.mozilla.org/es/docs/Web/JavaScript/Reference/Global_Objects/Date)
- [Moment.js](https://momentjs.com/)