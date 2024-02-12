---
title:                "Calcular una fecha en el futuro o pasado"
aliases:
- es/javascript/calculating-a-date-in-the-future-or-past.md
date:                  2024-01-20T17:31:17.779455-07:00
model:                 gpt-4-1106-preview
simple_title:         "Calcular una fecha en el futuro o pasado"

tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/javascript/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Qué y Por Qué?
Calcular una fecha en el futuro o en el pasado es simplemente ajustar la fecha actual para obtener otra. Los programadores lo hacen para manejar reservas, recordatorios, suscripciones y cualquier cosa que dependa del tiempo.

## Cómo Hacerlo:
Aquí hay un ejemplo simple: queremos saber la fecha 10 días a partir de ahora.

```javascript
// Obtener la fecha actual
let hoy = new Date();

// Calcular 10 días en milisegundos
let milisegundosPorDia = 24 * 60 * 60 * 1000;
let diezDiasEnMilisegundos = 10 * milisegundosPorDia;

// Crear una nueva fecha para 10 días más tarde
let fechaFutura = new Date(hoy.getTime() + diezDiasEnMilisegundos);

console.log(fechaFutura); // Muestra la fecha 10 días en el futuro
```

Si necesitas la fecha de hace 10 días, resta en lugar de sumar:

```javascript
let fechaPasada = new Date(hoy.getTime() - diezDiasEnMilisegundos);

console.log(fechaPasada); // Muestra la fecha 10 días en el pasado
```

## Análisis Profundo:
Calcular fechas en el futuro o pasado es esencial para trabajar con periodos de tiempo. JavaScript maneja fechas usando el objeto `Date`, que representa un único momento en tiempo universal coordinado (UTC) más un desplazamiento de zona horaria.

Antes, para realizar operaciones con fechas, era complicado y propenso a errores, pero con `Date` y librerías como Moment.js, las cosas son más sencillas. Sin embargo, muchas personas ahora prefieren librerías modernas como `date-fns` o `Day.js`, porque son más ligeras y modulares.

Detalles clave:
- JavaScript cuenta el tiempo en milisegundos desde el 1 de enero de 1970 (época Unix).
- Al trabajar con fechas, debes considerar años bisiestos y zonas horarias.
- Es posible encadenar métodos para formatos y cálculos más complejos.

## Vea También:
- [MDN - Date](https://developer.mozilla.org/es/docs/Web/JavaScript/Reference/Global_Objects/Date): documentación exhaustiva sobre el objeto `Date`.
- [Day.js](https://day.js.org/): una librería liviana para manipular fechas.
- [date-fns](https://date-fns.org/): librería moderna para trabajar con fechas en JavaScript.
