---
title:    "TypeScript: Obteniendo la fecha actual"
keywords: ["TypeScript"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/es/typescript/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Por qué

Una tarea común en cualquier programa es obtener la fecha y la hora actual. Esto puede ser útil para mostrar la hora actual en una página web, registrar timestamps en una base de datos, o simplemente como parte de un proceso de lógica de negocio. En esta publicación, aprenderemos cómo obtener la fecha actual en TypeScript y cómo manipularla para nuestros propósitos.

## Cómo hacerlo

La forma más sencilla de obtener la fecha actual en TypeScript es usando el objeto `Date`. Este objeto se puede crear sin argumentos, lo que dará como resultado la fecha y hora actuales del sistema:

```TypeScript
let fechaActual = new Date();
console.log(fechaActual);
```
Este código imprimirá la fecha y la hora actuales en la consola en el formato `ddd mmm dd yyyy hh:mm:ss GMT+TZ (hh:mm)`, donde `ddd` indica el día de la semana, `mmm` el mes, `dd` el día, `yyyy` el año, `hh:mm:ss` la hora en formato de 24 horas, `GMT` la zona horaria y `TZ` el desplazamiento de la zona horaria.

Podemos obtener valores específicos de la fecha y la hora de esta forma, usando los métodos `getDate()`, `getMonth()`, `getFullYear()`, `getHours()`, `getMinutes()` y `getSeconds()` de la clase `Date`:

```TypeScript
let dia = fechaActual.getDate();
let mes = fechaActual.getMonth() + 1;
let ano = fechaActual.getFullYear();
let hora = fechaActual.getHours();
let minutos = fechaActual.getMinutes();
let segundos = fechaActual.getSeconds();

console.log(`Hoy es ${dia}/${mes}/${ano} y la hora actual es ${hora}:${minutos}:${segundos}`);
```

También podemos especificar una fecha y hora específica al crear el objeto `Date`. Esto se hace pasando argumentos separados para el día, mes, año, hora, minutos y segundos en ese orden. Por ejemplo, si queremos obtener la fecha de mi cumpleaños, que es el 3 de septiembre de 1990 a las 12:00 pm, podemos escribir lo siguiente:

```TypeScript
let fechaNacimiento = new Date(1990, 8, 3, 12, 0, 0);
console.log(`Nací el ${fechaNacimiento}`);
```

## Deep Dive

El objeto `Date` también nos permite manipular fechas y horas. Algunos de los métodos más comunes son `setDate()`, `setMonth()`, `setFullYear()`, `setHours()`, `setMinutes()` y `setSeconds()`, que nos permiten cambiar los valores de la fecha y hora actuales. También podemos usar el método `getTime()`, que nos devuelve el número de milisegundos desde el 1 de enero de 1970 hasta la fecha especificada. Este valor es útil para comparar fechas y ordenarlas.

También podemos formatear la salida de la fecha y la hora utilizando el objeto `Intl.DateTimeFormat()` y sus métodos `format()` y `formatToParts()`, que nos permiten especificar el idioma y formato que deseamos, entre otras opciones.

## Ver también

- Documentación oficial de TypeScript sobre el objeto `Date`: https://www.typescriptlang.org/docs/handbook/datetime.html
- Tutoriales de TypeScript en español: https://www.youtube.com/playlist?list=PLJbE2Yu2zumDzHg-_vXdvTHEnT_MYxuW9