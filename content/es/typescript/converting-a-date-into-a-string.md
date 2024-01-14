---
title:                "TypeScript: Convirtiendo una fecha en una cadena"
programming_language: "TypeScript"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/typescript/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Por qué

Existen muchas situaciones donde es necesario convertir una fecha en una cadena de texto en TypeScript. Algunos ejemplos pueden ser mostrar la fecha en un formato específico, guardar la fecha en una base de datos o enviarla a un servicio externo. A continuación, explicaremos cómo realizar esta conversión de manera sencilla y efectiva.

## Cómo hacerlo

En TypeScript, podemos utilizar el objeto Date para almacenar y manipular fechas. Sin embargo, cuando necesitamos convertirlo en una cadena de texto, podemos utilizar el método toDateString().

```TypeScript
let today = new Date();
let dateString = today.toDateString();
console.log(dateString); // Mon Jun 14 2021
```

Podemos ver que el método toDateString() nos devuelve una cadena de texto con el formato predeterminado de la fecha. Pero, ¿qué pasa si queremos un formato específico como "DD/MM/YYYY"? En ese caso, podemos utilizar el método toLocaleDateString() y especificar el idioma y el formato deseado.

```TypeScript
let today = new Date();
let options = { day: '2-digit', month: '2-digit', year: 'numeric' };
let dateString = today.toLocaleDateString('es-ES', options);
console.log(dateString); // 14/06/2021
```

## Deep Dive

Si queremos tener un control más detallado sobre cómo se muestra la fecha, podemos utilizar el objeto Intl.DateTimeFormat. Este objeto nos permite personalizar completamente el formato de la fecha, incluyendo el idioma, el tipo de calendario y las opciones de formato.

```TypeScript
let today = new Date();
let options = { weekday: 'long', year: 'numeric', month: 'long', day: 'numeric' };
let format = new Intl.DateTimeFormat('es-ES', options);

console.log(format.format(today)); // lunes, 14 de junio de 2021
```

También podemos utilizar la librería moment.js, una herramienta muy popular para trabajar con fechas en TypeScript. Esta librería nos permite realizar conversiones de fechas de manera más sencilla y ofrece una amplia gama de formatos y opciones de personalización.

## Ver también

- [Documentación de Date en TypeScript](https://www.typescriptlang.org/docs/handbook/date-and-time.html)
- [Documentación de Intl.DateTimeFormat](https://developer.mozilla.org/es/docs/Web/JavaScript/Reference/Global_Objects/DateTimeFormat)
- [Documentación de moment.js](https://momentjs.com/docs/)