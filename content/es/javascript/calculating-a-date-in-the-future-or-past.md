---
title:                "Calculando una fecha en el futuro o pasado"
html_title:           "Javascript: Calculando una fecha en el futuro o pasado"
simple_title:         "Calculando una fecha en el futuro o pasado"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/javascript/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## ¿Qué y Por qué?

El cálculo de una fecha futura o pasada en Javascript permite manipular fechas en relación con el tiempo actual. Los programadores lo hacen para funciones como temporizadores, seguimiento del tiempo transcurrido, o planificación de eventos.

## ¿Cómo lo hago?

Usaremos el objeto Date de JavaScript para este propósito. Vamos a crear una nueva fecha y luego agregar o restar días de ella.

```Javascript
// Crear una nueva fecha
let fecha = new Date();

// Mostrar la fecha actual
console.log(fecha);

// Calcular una fecha futura (Agregar 5 días)
fecha.setDate(fecha.getDate() + 5);
console.log(fecha);

// Calcular una fecha pasada (Restar 3 días)
fecha.setDate(fecha.getDate() - 3);
console.log(fecha);
```
Si corres el código, se mostrarán tres fechas: la fecha actual, fecha futura (5 días desde ahora), y fecha pasada (dos días antes de la fecha futura).

## Inmersión Profunda

1. Historia: JavaScript se lanzó en 1995 y el objeto Date fue parte de la biblioteca estándar desde el principio, permitiendo a los desarrolladores trabajar con fechas y horas.
2. Alternativas: Si bien la forma mostrada es directa, hay bibliotecas como Moment.js y Day.js que proporcionan una interfaz más rica para trabajar con fechas.
3. Detalles de implementación: El método setDate modifica el día del mes del objeto Date en base al tiempo local, no el tiempo UTC.

## Ver También

Para más detalles sobre el manejo de fechas en Javascript, puedes visitar las siguientes fuentes:

1. Documentación oficial de Mozilla sobre el objeto Fecha ([Date - JavaScript | MDN](https://developer.mozilla.org/es/docs/Web/JavaScript/Reference/Global_Objects/Date))
2. Biblioteca Moment.js ([Moment.js](https://momentjs.com/))
3. Biblioteca Day.js ([Day.js](https://day.js.org/))