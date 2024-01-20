---
title:                "Calculando una fecha en el futuro o pasado"
html_title:           "TypeScript: Calculando una fecha en el futuro o pasado"
simple_title:         "Calculando una fecha en el futuro o pasado"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/typescript/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## ¿Qué y por qué?
Calcular una fecha en el futuro o en el pasado significa manipular los valores de datetimes en tu código. Los programadores usualmente lo hacen para programar eventos, recordatorios, o cálculos de tiempo entre fechas.

## Cómo hacerlo:

Para calcular una fecha en el futuro en TypeScript, puedes usar el objeto `Date` y sus métodos. Aquí tienes un ejemplo de cómo agregar un número de días a la fecha actual:

```TypeScript
let hoy = new Date();
let diasParaAgregar = 5;

hoy.setDate(hoy.getDate() + diasParaAgregar);

console.log(hoy);
```

En este ejemplo, creamos un nuevo objeto `Date`, que representa la fecha actual por defecto. Luego agregamos 5 días a esa fecha usando `setDate()`, que toma el valor actual del día del objeto date y le suma el número de días que quieres agregar.

Para calcular una fecha en el pasado, simplemente resta los días:

```TypeScript
let hoy = new Date();
let diasParaRestar = 5;

hoy.setDate(hoy.getDate() - diasParaRestar);

console.log(hoy);
```

## Más detalles:

Históricamente, calcular fechas en el pasado o el futuro era más complicado antes de que los objetos de fecha y hora incorporados se volvieran comunes en los lenguajes de programación. Antes, había que hacer un seguimiento manual de los días en cada mes, los años bisiestos, etc.

Una de las alternativas a usar el objeto `Date` nativo de TypeScript/Javascript es la biblioteca `moment.js`, que proporciona una API más rica para manipular fechas y tiempos. Sin embargo, `moment.js` puede ser excesiva para tareas sencillas. 

La implementación de los métodos de `Date` en TypeScript esencialmente envuelve los métodos equivalentes de JavaScript. Por ejemplo, `setDate()` cambia el día del mes del objeto `Date`, y automáticamente ajusta el mes y el año según sea necesario.

## Ver también:

1. La documentación oficial de TypeScript sobre el objeto `Date`: https://www.typescriptlang.org/docs/handbook/declaration-files/do-s-and-don-ts.html
2. Moment.js, una popular biblioteca de fechas y horas: https://momentjs.com/
3. Fecha y hora en MDN (Mozilla Developer Network): https://developer.mozilla.org/es/docs/Web/JavaScript/Referencia/Objetos_globales/Date