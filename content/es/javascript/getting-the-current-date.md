---
title:                "Obteniendo la fecha actual"
html_title:           "Javascript: Obteniendo la fecha actual"
simple_title:         "Obteniendo la fecha actual"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/javascript/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Por qué

¿Alguna vez has necesitado saber la fecha y hora exacta en la que se está ejecutando tu código? ¡Obtener la fecha y hora actual es crucial para muchas aplicaciones y tareas de programación! Afortunadamente, en Javascript hay una manera sencilla de obtener la fecha actual y usarla en tu código.

## Cómo hacerlo

Obtener la fecha actual en Javascript es bastante sencillo. Solo necesitas usar el método `Date()` de la siguiente manera:

```Javascript
let fechaActual = new Date();
```

Este código crea un objeto `fechaActual` que contiene la fecha y hora actuales en una instancia de `Date`.

Si quieres obtener la fecha y hora específica de una zona horaria en particular, puedes usar el método `toLocaleString()` junto con el objeto `Date()` de la siguiente manera:

```Javascript
let fechaActual = new Date().toLocaleString("es-CO", { timeZone: "America/Bogota" });
```

Este código obtendrá la fecha y hora en formato de cadena de texto en la zona horaria especificada (en este caso, Colombia).

## Profundizando

¿Qué pasa si quieres obtener solo la fecha o solo la hora actual? En Javascript, puedes usar una variedad de métodos para acceder a diferentes partes de un objeto `Date()`. Algunos de ellos son:

- `.getFullYear()`: Devuelve el año completo de la fecha.
- `.getMonth()`: Devuelve el mes actual (comenzando desde 0 para enero).
- `.getDate()`: Devuelve el día del mes.
- `.getHours()`: Devuelve la hora actual.
- `.getMinutes()`: Devuelve los minutos actuales.
- `.getSeconds()`: Devuelve los segundos actuales.

Estos son solo algunos de los métodos disponibles para acceder a diferentes partes de una fecha. Puedes consultar la documentación de MDN para ver la lista completa.

## Ver también

- [Documentación de Date en MDN](https://developer.mozilla.org/es/docs/Web/JavaScript/Reference/Global_Objects/Date)
- [Más formas de obtener la fecha actual en Javascript](https://www.codegrepper.com/code-examples/javascript/get+current+date+and+time+javascript)
- [Cómo trabajar con fechas y horas en Javascript](https://www.w3schools.com/js/js_dates.asp)