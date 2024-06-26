---
date: 2024-01-20 17:32:12.450417-07:00
description: "C\xF3mo Hacerlo: En los d\xEDas de JavaScript temprano, manejar fechas\
  \ era tedioso y propenso a errores. TypeScript, al ofrecer un ambiente de tipado\
  \ fuerte, ha\u2026"
lastmod: '2024-04-05T21:54:00.163149-06:00'
model: gpt-4-1106-preview
summary: "En los d\xEDas de JavaScript temprano, manejar fechas era tedioso y propenso\
  \ a errores."
title: Calcular una fecha en el futuro o pasado
weight: 26
---

## Cómo Hacerlo:
```TypeScript
// Sumar días a la fecha actual
const hoy = new Date();
const enDiezDias = new Date(hoy.getTime() + (10 * 24 * 60 * 60 * 1000));
console.log(enDiezDias.toDateString()); // Muestra: La fecha de hoy + 10 días

// Restar días a la fecha actual
const haceCincoDias = new Date(hoy.getTime() - (5 * 24 * 60 * 60 * 1000));
console.log(haceCincoDias.toDateString());  // Muestra: La fecha de hoy - 5 días
```

## Profundizando
En los días de JavaScript temprano, manejar fechas era tedioso y propenso a errores. TypeScript, al ofrecer un ambiente de tipado fuerte, ha facilitado estas operaciones, pero se basa en los mismos objetos `Date` de JavaScript.

Alternativas como las librerías **Moment.js** y **date-fns** ofrecen más funciones y facilidades al manejar fechas, aunque TypeScript provee las herramientas básicas necesarias.

En el ejemplo, utilizamos el método `getTime()` que devuelve el tiempo en milisegundos desde la Unix Epoch (1 de enero de 1970). Al sumar o restar milisegundos a este valor, podemos movernos hacia adelante o atrás en el tiempo. Es crucial tener en cuenta las zonas horarias y el horario de verano para cálculos precisos.

## Véase También
- Documentación oficial de Date en MDN: [MDN Date](https://developer.mozilla.org/es/docs/Web/JavaScript/Reference/Global_Objects/Date)
- Librería **Moment.js**: [Moment.js](https://momentjs.com/)
- Librería **date-fns**: [date-fns](https://date-fns.org/)
