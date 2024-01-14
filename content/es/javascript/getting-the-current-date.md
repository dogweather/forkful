---
title:    "Javascript: Obteniendo la fecha actual"
keywords: ["Javascript"]
---

{{< edit_this_page >}}

## Por qué obtener la fecha actual es importante en la programación de Javascript

La fecha actual es una parte importante de cualquier programa que involucre funciones de tiempo en Javascript. Puede ser útil para crear registros de tiempo, programar eventos y tareas, o simplemente para mostrar la fecha actual en una aplicación. Con la ayuda de algunos consejos y trucos, obtener la fecha actual en Javascript es fácil y puede mejorar la funcionalidad de sus programas.

## Cómo obtener la fecha actual en Javascript

La forma más sencilla de obtener la fecha actual en Javascript es utilizando el objeto Date. Puede crear una instancia de este objeto y llamar a sus métodos para obtener la fecha actual en diferentes formatos. Por ejemplo, el siguiente código imprimirá la fecha actual en el formato "aa/mm/dd":

```Javascript
// Crear una instancia del objeto Date
var fecha = new Date();

// Llamar al método getDate para obtener el día del mes
var dia = fecha.getDate();

// Llamar al método getMonth para obtener el mes (teniendo en cuenta que los meses en Javascript empiezan en 0)
var mes = fecha.getMonth() + 1;

// Llamar al método getFullYear para obtener el año
var año = fecha.getFullYear();

// Imprimir la fecha actual en el formato deseado
console.log(año + "/" + mes + "/" + dia);
```

El código anterior imprimirá algo similar a esto: "2021/06/13". Sin embargo, también puede utilizar otros métodos como getDay() para obtener el día de la semana, getHours() para obtener la hora actual o incluso getTime() para obtener el tiempo en milisegundos desde el 1 de enero de 1970. Puede ver una lista completa de métodos disponibles en la documentación oficial de Javascript.

## Profundizando en la obtención de la fecha actual en Javascript

Hay algunas cosas importantes que debe tener en cuenta al obtener la fecha actual en Javascript. Primero, tenga en cuenta que la fecha y la hora son valores locales, por lo que pueden variar según la ubicación del usuario. Además, asegúrese de ajustar el mes y el día para que tengan dos dígitos en lugar de uno para una mejor presentación.

Otra cosa importante es la diferencia entre "getMonth" y "getUTCMonth". Mientras que "getMonth" devuelve el mes local (0-11), "getUTCMonth" devuelve el mes en formato UTC (0-11). Esto es importante si desea obtener la fecha y hora en un formato específico de zona horaria.

Por último, puede utilizar librerías como moment.js o date-fns para facilitar la manipulación y formato de fechas en Javascript.

## Ver también

- Documentación oficial de Javascript: https://developer.mozilla.org/es/docs/Web/JavaScript/Reference/Global_Objects/Date
- Tutorial de w3schools sobre cómo obtener la fecha y hora en Javascript: https://www.w3schools.com/js/js_date_methods.asp
- Documentación de moment.js: https://momentjs.com/
- Documentación de date-fns: https://date-fns.org/