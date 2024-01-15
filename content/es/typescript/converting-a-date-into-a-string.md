---
title:                "Convirtiendo una fecha en una cadena"
html_title:           "TypeScript: Convirtiendo una fecha en una cadena"
simple_title:         "Convirtiendo una fecha en una cadena"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/typescript/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Por qué

Convertir una fecha en una cadena de texto es una habilidad esencial cuando se trabaja con fechas en TypeScript. Puede ser necesario para mostrar las fechas en un formato específico en una aplicación o para realizar cálculos basados en fechas. En este artículo, aprenderemos cómo convertir fácilmente una fecha en una cadena de texto en TypeScript.

## Cómo hacerlo

Para convertir una fecha en una cadena de texto en TypeScript, podemos utilizar el método `toString()` y especificar el formato deseado utilizando métodos auxiliares de la clase `Date`.

Veamos un ejemplo de código:

```TypeScript
let today = new Date();

//Convertir la fecha en una cadena en formato dd/mm/aaaa
let dateString = today.getDate() + "/" + (today.getMonth() + 1) + "/" + today.getFullYear();

console.log(dateString); //Output: 03/01/2022
```

En este ejemplo, hemos creado una nueva instancia de la clase `Date` y luego utilizamos los métodos `getDate()`, `getMonth()` y `getFullYear()` para obtener el día, mes y año de la fecha actual. Al concatenar estas variables con "/" como separadores, podemos obtener una cadena de texto en el formato deseado.

Otra forma de convertir una fecha en una cadena es utilizando el método `toLocaleDateString()`, que nos permite especificar el idioma y el formato de la fecha. Por ejemplo:

```TypeScript
let today = new Date();

//Convertir la fecha en una cadena en formato 03 de enero de 2022
let dateString = today.toLocaleDateString("es-ES", {day: 'numeric', month: 'long', year: 'numeric'});

console.log(dateString); //Output: 03 de enero de 2022
```

En este caso, hemos especificado el idioma español (es-ES) y el formato de día, mes y año en formato numérico y largo. Esto nos devuelve una cadena en el formato deseado en español.

## Deep Dive

Para aquellos interesados en aprender más sobre cómo convertir una fecha en una cadena en TypeScript, es importante entender que:

- El método `toString()` devuelve la fecha en el formato completo (por ejemplo, "Mon Jan 03 2022 00:00:00 GMT-0500 (hora estándar oriental)").
- El método `toLocaleString()` también nos permite especificar el formato de la hora y la zona horaria además de la fecha.

## Ver también

- [Documentación oficial de TypeScript sobre fechas](https://www.typescriptlang.org/docs/handbook/functions.html#the-void-type)
- [Convertir una cadena en formato de fecha en TypeScript](https://www.tutorialspoint.com/typescript/typescript_date.htm)