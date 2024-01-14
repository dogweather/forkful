---
title:                "Javascript: Obteniendo la fecha actual"
simple_title:         "Obteniendo la fecha actual"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/javascript/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Por qué

La obtención de la fecha actual es una tarea común en la programación. Puede ser útil para realizar cálculos de tiempo, registrar eventos o simplemente mostrar la fecha en una interfaz de usuario. Afortunadamente, Javascript tiene una función incorporada que nos permite obtener la fecha actual con facilidad.

## Cómo hacerlo

Para obtener la fecha actual en Javascript, podemos utilizar la función `Date()`. Esta función devuelve un objeto de fecha que contiene la fecha y hora actuales. Veamos un ejemplo:

```Javascript
let fechaActual = new Date();
console.log(fechaActual);
```

La salida de este código sería una cadena de texto que representa la fecha y hora actuales en el formato `Día Mes Año Hora:Minutos:Segundos`. Por ejemplo: `Fri Sep 24 2021 21:55:30`.

Podemos obtener partes específicas de la fecha, como el día, mes, año, hora, minutos y segundos de la siguiente manera:

```Javascript
let fechaActual = new Date();
console.log(fechaActual.getDate()); // retorna el día del mes
console.log(fechaActual.getMonth()); // retorna el mes (cuenta desde 0)
console.log(fechaActual.getFullYear()); // retorna el año
console.log(fechaActual.getHours()); // retorna la hora
console.log(fechaActual.getMinutes()); // retorna los minutos
console.log(fechaActual.getSeconds()); // retorna los segundos
```

Esta función también es útil para comparar fechas y realizar cálculos de tiempo. Por ejemplo, si queremos saber cuántos días han pasado desde una fecha específica, podemos hacer lo siguiente:

```Javascript
let fechaInicial = new Date(2021, 0, 1); // 1 de enero de 2021
let fechaActual = new Date();
let diferenciaEnDias = (fechaActual - fechaInicial) / (1000 * 60 * 60 * 24); //conversión de milisegundos a días
console.log(`Ha pasado ${diferenciaEnDias} días desde el 1 de enero de 2021.`)
```

La función `Date()` también acepta argumentos para obtener una fecha específica en lugar de la fecha actual. Por ejemplo, `new Date(2021, 9, 16)` nos devolvería la fecha 16 de octubre de 2021.

## Profundizando

El objeto de fecha en Javascript también tiene métodos que nos permiten establecer y modificar fechas. Por ejemplo, `setDate()` nos permite establecer el día del mes, `setMonth()` nos permite establecer el mes (cuenta desde 0) y `setFullYear()` nos permite establecer el año. También hay métodos para establecer la hora, minutos, segundos y milisegundos.

También podemos formatear la fecha en diferentes formatos utilizando los métodos `toDateString()`, `toLocaleDateString()` y `toISOString()`. Estos métodos nos permiten obtener la fecha en un formato más legible para los humanos o en un formato estandarizado.

Otra cosa a tener en cuenta es que la función `Date()` se basa en la hora local del navegador del usuario. Si queremos obtener la fecha y hora en una zona horaria específica, podemos utilizar la librería Moment.js o Timezone.js.

## Ver también

- [Documentación de MDN sobre la función Date()](https://developer.mozilla.org/es/docs/Web/JavaScript/Referencia/Objetos_globales/Date)
- [Tutorial de W3Schools sobre cómo trabajar con fechas en Javascript](https://www.w3schools.com/js/js_dates.asp)
- [Moment.js](https://momentjs.com/)
- [Timezone.js](https://github.com/moment/timezone)