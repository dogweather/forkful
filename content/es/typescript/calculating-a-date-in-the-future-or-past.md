---
title:                "TypeScript: Calculando una fecha en el futuro o en el pasado."
programming_language: "TypeScript"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/typescript/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Por qué

Calcular fechas en el futuro o pasado puede ser extremadamente útil en programación ya que permite automatizar tareas que dependen de fechas específicas. También es útil en situaciones donde se necesita predecir eventos futuros o hacer cálculos temporales.

## Cómo hacerlo

La manera más sencilla de calcular una fecha en TypeScript es utilizando la clase `Date`. Esta clase nos permite crear objetos que representan una fecha y realizar operaciones con ellas. A continuación, se muestra un ejemplo de cómo obtener la fecha actual y sumarle un día:

```
TypeScript
let fechaActual = new Date();
fechaActual.setDate(fechaActual.getDate() + 1);
console.log(fechaActual); // salida: Wed Nov 04 2020 15:54:41 GMT-0300 (hora del Pacífico de Sudamérica)
```

En este ejemplo, primero creamos un objeto `Date` que representa la fecha actual y luego utilizamos el método `setDate()` para sumar un día a la fecha. Finalmente, imprimimos la nueva fecha en la consola.

También podemos calcular fechas en el pasado utilizando valores negativos en los argumentos del método `setDate()`. Por ejemplo, para restar un año a la fecha actual:

```
TypeScript
let fechaActual = new Date();
fechaActual.setDate(fechaActual.getDate() - 365);
console.log(fechaActual); // salida: Tue Oct 22 2019 15:59:47 GMT-0300 (hora del Pacífico de Sudamérica)
```

Otra forma de calcular fechas en TypeScript es utilizando la librería `moment.js`. Esta librería nos brinda métodos más intuitivos para realizar operaciones con fechas. A continuación, se muestra un ejemplo utilizando `moment.js` para sumar un mes a la fecha actual:

```
TypeScript
import * as moment from 'moment'
let fechaActual = moment();
fechaActual.add(1, 'months');
console.log(fechaActual.toDate()); // salida: Thu Dec 03 2020 16:07:58 GMT-0300 (hora del Pacífico de Sudamérica)
```

## Profundizando

Al calcular fechas en el futuro o pasado, es importante tener en cuenta que algunas fechas pueden ser "saltadas" o "duplicadas" debido a cambios en el horario de verano, por ejemplo. Por lo tanto, es recomendable utilizar métodos que tengan en cuenta estos cambios, como `moment.utc()` de la librería `moment.js`.

Además, es importante tener en cuenta que las fechas en JavaScript comienzan en enero con el valor 0, por lo que febrero sería el mes 1 y diciembre el mes 11.

## Ver también

- [Documentación de la clase Date en TypeScript](https://developer.mozilla.org/es/docs/Web/JavaScript/Referencia/Objetos_globales/Date)
- [Documentación de moment.js en TypeScript](https://momentjs.com/docs/)
- [Cómo manejar fechas y horarios en TypeScript](https://www.digitalocean.com/community/tutorials/manejo-de-fechas-y-horas-en-typescript-es)