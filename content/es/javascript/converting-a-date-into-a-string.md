---
title:    "Javascript: Convirtiendo una fecha en una cadena"
keywords: ["Javascript"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/es/javascript/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Por qué

Convertir una fecha en una cadena de texto es una tarea común y necesaria en programación. Ya sea para mostrar la fecha en un formato específico o para realizar cálculos con fechas, es importante saber cómo convertir una fecha en una cadena.

## Cómo hacerlo

Para convertir una fecha en una cadena de texto en Javascript, primero necesitas tener una fecha de partida, ya sea en formato de objeto de fecha o en milisegundos. A continuación, puedes utilizar el método `toISOString()` para obtener la fecha en formato ISO, o `toLocaleString()` para obtener la fecha en formato local.

```Javascript
// Fecha como objeto de fecha
var fecha = new Date("2020-01-01");

// Obtener fecha en formato ISO
console.log(fecha.toISOString()); // Output: 2020-01-01T00:00:00.000Z

// Obtener fecha en formato local
console.log(fecha.toLocaleString()); // Output: 1/1/2020, 12:00:00 AM
```

También puedes utilizar la librería Moment.js para tener más control sobre el formato de la fecha. Por ejemplo:

```Javascript
// Fecha como objeto de fecha
var fecha = moment("2020-01-01");

// Obtener fecha en formato personalizado
console.log(fecha.format("DD/MM/YYYY")); // Output: 01/01/2020
```

## Profundizando

Convertir una fecha en una cadena de texto puede ser un poco confuso cuando se trata de zonas horarias. Por ejemplo, si usas el método `toISOString()` para una fecha en formato UTC y luego lo conviertes de nuevo a un objeto de fecha, obtendrás una fecha que se ha convertido a tu zona horaria local. Para evitar esto, puedes utilizar el método `toJSON()` en lugar de `toISOString()`.

```Javascript
// Fecha como objeto de fecha
var fecha = new Date("2020-01-01");

// Convertir a formato JSON y luego a objeto de fecha
var fechaJSON = fecha.toJSON();
var fechaObjeto = new Date(fechaJSON);

// Comparar las dos fechas
console.log(fecha); // Output: Wed Jan 01 2020 00:00:00 GMT+0100 (hora estándar de Europa central)
console.log(fechaObjeto); // Output: Wed Jan 01 2020 02:00:00 GMT+0100 (hora estándar de Europa central)
```

## Ver también

- [Documentación de Date en MDN](https://developer.mozilla.org/es/docs/Web/JavaScript/Reference/Global_Objects/Date) 
- [Moment.js documentation](https://momentjs.com/docs/) 
- [Convertir fecha a formato ISO en Javascript](https://www.w3schools.com/jsref/tryit.asp?filename=tryjsref_toisostring)