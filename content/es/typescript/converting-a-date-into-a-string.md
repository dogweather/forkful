---
title:    "TypeScript: Convirtiendo una fecha en una cadena"
keywords: ["TypeScript"]
---

{{< edit_this_page >}}

## Por qué

En programación, a menudo necesitamos trabajar con fechas y mostrarlas en diferentes formatos. Convertir una fecha en una cadena de texto puede ser útil cuando queremos presentar la información de manera más legible para el usuario final.

## Cómo hacerlo

Para convertir una fecha en una cadena de texto en TypeScript, podemos utilizar el método `toString()` de la clase `Date`. Veamos un ejemplo:

```TypeScript
let fecha = new Date();
let fechaString = fecha.toString();
console.log(fechaString); // Mié Jun 30 2021 22:50:48 GMT-0500 (hora de verano central)
```

También podemos especificar el formato de fecha que queremos mostrar utilizando los métodos `getFullYear()`, `getMonth()` y `getDate()`. Por ejemplo:

```TypeScript
let fecha = new Date();
let año = fecha.getFullYear();
let mes = fecha.getMonth();
let día = fecha.getDate();

let fechaString = `${día}/${mes}/${año}`;
console.log(fechaString); // 30/5/2021
```

## Profundizando

Si queremos ser más precisos en la conversión de una fecha en una cadena de texto, podemos utilizar bibliotecas externas como Moment.js. Esta biblioteca nos permite dar formato a las fechas según nuestras necesidades y también trabajar fácilmente con múltiples zonas horarias.

Otra consideración importante es la localización. Dependiendo del país o idioma del usuario final, las fechas pueden ser representadas de manera diferente. En TypeScript, podemos utilizar el objeto `Intl.DateTimeFormat` para manejar estas diferencias locales.

## Ver también

- [Documentación de Date en TypeScript](https://www.typescriptlang.org/docs/handbook/standard-library.html#date)
- [Moment.js](https://momentjs.com/)
- [Manipulación de fechas con TypeScript](https://www.digitalocean.com/community/tutorials/manipular-fechas-utilizando-moment-js-es)