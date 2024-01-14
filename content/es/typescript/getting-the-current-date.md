---
title:                "TypeScript: Obteniendo la fecha actual"
programming_language: "TypeScript"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/typescript/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Por qué

Obtener la fecha actual es una tarea común en cualquier lenguaje de programación. Ya sea para mostrar la fecha en una página web, registrar eventos o generar nombres de archivo únicos, es importante saber cómo obtener la fecha actual en TypeScript.

## Cómo

Para obtener la fecha actual en TypeScript, podemos utilizar el método `Date()` que devuelve un objeto `Date`. Luego, podemos utilizar varios métodos del objeto `Date` para obtener y formatear la fecha según nuestros requisitos. Veamos algunos ejemplos:

```TypeScript
const currentDate = new Date();
console.log(currentDate);
```

Este código creará una variable `currentDate` que contiene un objeto `Date` con la hora y fecha actuales en el formato siguiente: 

```
Mon Aug 09 2021 15:14:09 GMT-0400 (hora de verano oriental)
```

También podemos utilizar métodos específicos del objeto `Date` para obtener partes específicas de la fecha, como el día, el mes y el año.

```TypeScript
const currentMonth = currentDate.getMonth() + 1; // El +1 es necesario ya que los meses en JavaScript están indexados desde 0
const currentDay = currentDate.getDate();
const currentYear = currentDate.getFullYear();

console.log(`Hoy es ${currentDay}/${currentMonth}/${currentYear}.`);
```

Este ejemplo imprimirá la fecha actual en el formato "día/mes/año", por ejemplo: `Hoy es 09/08/2021.`

También podemos formatear la hora utilizando los métodos `getHours()` y `getMinutes()`.

```TypeScript
const currentHour = currentDate.getHours();
const currentMinute = currentDate.getMinutes();

console.log(`Son las ${currentHour}:${currentMinute} en punto.`);
```

Este código imprimirá la hora actual en el formato "hora:minuto", por ejemplo: `Son las 15:21 en punto.`

## Profundizando

El objeto `Date` en TypeScript también tiene métodos para trabajar con fechas anteriores o posteriores a la fecha actual, así como para comparar fechas y realizar cálculos con ellas. También podemos utilizar bibliotecas externas como Moment.js para realizar tareas más complejas relacionadas con fechas.

## Ver también
- [Documentación de Date en TypeScript](https://developer.mozilla.org/es/docs/Web/JavaScript/Referencia/Objetos_globales/Date)
- [Tutorial de Moment.js](https://www.codecademy.com/es/tracks/moment-js)
- [Artículo sobre formatting de fechas en TypeScript](https://www.digitalocean.com/community/tutorials/javascript-dates-formatting-using-momentjs)