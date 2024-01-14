---
title:                "TypeScript: Convirtiendo una fecha en un texto"
simple_title:         "Convirtiendo una fecha en un texto"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/typescript/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Por qué

Hay varias razones por las que podrías querer convertir una fecha en una cadena de texto en TypeScript. Algunos ejemplos incluyen mostrar la fecha en un formato específico para el usuario, almacenar la fecha en una base de datos o enviarla en un formato determinado a una API.

## Cómo hacerlo

Para convertir una fecha en TypeScript en una cadena de texto, puedes utilizar el método `toISOString()` de la clase `Date`. Este método convierte la fecha en una cadena en formato ISO, que es ampliamente utilizado en aplicaciones web.

```
TypeScript
const fecha = new Date();
const fechaString = fecha.toISOString();
console.log(fechaString); // Output: '2021-04-12T15:35:00.000Z'
```

También puedes utilizar el método `toDateString()` si solo quieres mostrar la fecha en formato de cadena sin la hora y la zona horaria.

```
TypeScript
const fecha = new Date();
const fechaString = fecha.toDateString();
console.log(fechaString); // Output: 'Mon Apr 12 2021'
```

Para mostrar la fecha en un formato personalizado, puedes utilizar el objeto `Intl` y su método `DateTimeFormat()`. Este método te permite especificar el idioma, el formato y las opciones de la fecha.

```
TypeScript
const fecha = new Date();
const options = { weekday: 'long', year: 'numeric', month: 'long', day: 'numeric' };
const fechaString = new Intl.DateTimeFormat('es-ES', options).format(fecha);
console.log(fechaString); // Output: 'lunes 12 de abril de 2021'
```

## Profundizando

La clase `Date` en JavaScript y TypeScript almacena las fechas y las horas como un número de milisegundos desde el 1 de enero de 1970 a las 00:00:00 UTC. Al convertir una fecha a una cadena, es importante tener en cuenta el formato que se usará y cómo será interpretado por otros sistemas o usuarios.

Además, si deseas trabajar con diferentes husos horarios, necesitarás utilizar la librería `moment.js` o el objeto `Intl` con la opción `timeZone` para especificar la zona horaria deseada en la conversión.

## Ver también

- [Documentación de Date en TypeScript](https://www.typescriptlang.org/docs/handbook/release-notes/typescript-2-2.html#working-with-dates)
- [Documentación de Intl en MDN](https://developer.mozilla.org/es/docs/Web/JavaScript/Reference/Global_Objects/DateTimeFormat)
- [Documentación de moment.js](https://momentjs.com/docs/)