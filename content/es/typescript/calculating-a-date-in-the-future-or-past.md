---
title:                "Calcular una fecha en el futuro o pasado"
date:                  2024-01-20T17:32:12.450417-07:00
model:                 gpt-4-1106-preview
simple_title:         "Calcular una fecha en el futuro o pasado"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/typescript/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## ¿Qué & Por Qué?
Calcular fechas futuras o pasadas significa determinar un día y hora específicos antes o después de una fecha dada. Los programadores lo hacen para manejar eventos, suscripciones, recordatorios, y cualquier cosa que necesite seguimiento temporal.

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