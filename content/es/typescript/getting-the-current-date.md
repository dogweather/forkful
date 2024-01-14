---
title:    "TypeScript: Obteniendo la fecha actual"
keywords: ["TypeScript"]
---

{{< edit_this_page >}}

## Por qué obtener la fecha actual es útil

Obtener la fecha actual es una tarea común en muchos proyectos de programación. Saber la fecha y hora exactas puede ser útil para muchas cosas, como registrar el momento en que se realizó una acción, generar facturas con la fecha actual o simplemente mostrar la fecha al usuario.

## Cómo obtener la fecha actual en TypeScript

Obtener la fecha actual en TypeScript es muy sencillo gracias a la clase `Date`. Esta clase viene incorporada en TypeScript y nos permite crear objetos con la fecha y hora actuales. Podemos acceder a diferentes propiedades de este objeto, como el año, mes, día, hora, minutos, segundos y milisegundos. 

Para obtener la fecha actual, simplemente debemos crear un nuevo objeto `Date` y luego utilizar las diferentes propiedades para obtener la información que necesitamos. Por ejemplo:

```TypeScript
const fechaActual = new Date();

console.log(fechaActual); // Output: Wed Sep 01 2021 14:35:49 GMT-0500 (hora estándar de Perú)

console.log(fechaActual.getFullYear()); // Output: 2021
console.log(fechaActual.getMonth()); // Output: 8 (Septiembre, los meses empiezan en 0)
console.log(fechaActual.getDate()); // Output: 1
console.log(fechaActual.getHours()); // Output: 14
console.log(fechaActual.getMinutes()); // Output: 35
console.log(fechaActual.getSeconds()); // Output: 49
```

Si queremos mostrar la fecha en un formato específico, podemos utilizar el método `toLocaleString()` junto con algunos parámetros. Por ejemplo, si queremos mostrar la fecha en formato `dd/mm/yyyy`, podemos hacer lo siguiente:

```TypeScript
const fechaActual = new Date();

const dia = fechaActual.getDate();
const mes = fechaActual.getMonth() + 1;
const año = fechaActual.getFullYear();

console.log(dia + '/' + mes + '/' + año); // Output: 01/09/2021
```

## Un poco más profundo

Para aquellos que quieren entender un poco más sobre cómo funciona la clase `Date` y cómo se calcula la fecha actual, podemos decir que esta clase utiliza un contador interno que almacena la cantidad de milisegundos transcurridos desde el 1 de enero de 1970. Este contador se actualiza cada vez que se crea un nuevo objeto `Date`, por lo que siempre tendremos la fecha y hora exactas en el momento de la creación.

Además, la clase `Date` también nos permite trabajar con fechas en el futuro o en el pasado, ya que podemos pasarle parámetros para crear objetos con una fecha y hora específicas. Por ejemplo, si queremos obtener la fecha del 25 de diciembre de 2021, podemos hacer lo siguiente:

```TypeScript
const navidad2021 = new Date(2021, 11, 25);

console.log(navidad2021); // Output: Sat Dec 25 2021 00:00:00 GMT-0500 (hora estándar de Perú)
```

## Ver también

- [Documentación oficial de la clase Date en TypeScript](https://www.typescriptlang.org/docs/handbook/intro.html)
- [Ejemplo de uso de la clase Date en una aplicación web con TypeScript](https://www.digitalocean.com/community/tutorials/how-to-work-with-dates-and-times-in-typescript)
- [Otras formas de obtener la fecha actual en TypeScript](https://www.freecodecamp.org/news/how-to-get-the-current-date-and-time-in-javascript/)