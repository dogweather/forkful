---
title:    "Javascript: Obteniendo la fecha actual"
keywords: ["Javascript"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/es/javascript/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Por qué

Uno de los conceptos fundamentales en programación es el manejo de fechas y horas. La obtención de la fecha y hora actual es uno de los puntos de partida para realizar diferentes tareas, como registros de tiempo, programación de eventos, entre otros. Además, conocer la fecha y hora actual también es útil para mostrar información actualizada a los usuarios.

## Cómo hacerlo

Para obtener la fecha y hora actual en JavaScript, podemos utilizar el objeto `Date` y sus respectivos métodos. Aquí te mostramos un ejemplo de cómo obtener la fecha actual en formato estándar:

```Javascript
const fechaActual = new Date();
let dia = fechaActual.getDate();
let mes = fechaActual.getMonth() + 1;
let año = fechaActual.getFullYear();
console.log(`La fecha actual es: ${dia}/${mes}/${año}`);
```

La salida de este código sería:
```
La fecha actual es: 14/6/2021
```

Además, podemos obtener la hora actual utilizando los métodos `getHours()`, `getMinutes()` y `getSeconds()` del objeto `Date`.

```Javascript
const horaActual = new Date();
let horas = horaActual.getHours();
let minutos = horaActual.getMinutes();
let segundos = horaActual.getSeconds();
console.log(`La hora actual es: ${horas}:${minutos}:${segundos}`);
```

La salida sería:
```
La hora actual es: 23:30:45
```

## Profundizando

Como mencionamos anteriormente, el objeto `Date` tiene diferentes métodos para obtener información de la fecha y hora actual. Algunos de estos métodos son:

- `getDate()`: devuelve el día del mes (1-31)
- `getMonth()`: devuelve el mes (0-11)
- `getFullYear()`: devuelve el año (4 dígitos)
- `getHours()`: devuelve la hora (0-23)
- `getMinutes()`: devuelve los minutos (0-59)
- `getSeconds()`: devuelve los segundos (0-59)

También podemos obtener la fecha y hora en diferentes formatos, como hora militar o formato AM/PM, utilizando los métodos `getUTCHours()` y `toLocaleTimeString()`.

## Ver también

- [Documentación oficial de JavaScript sobre el objeto Date](https://developer.mozilla.org/es/docs/Web/JavaScript/Referencia/Objetos_globales/Date)
- [Cómo trabajar con fechas y horas en JavaScript](https://www.digitalocean.com/community/tutorials/js-working-with-dates)
- [Métodos de formato de fecha y hora en JavaScript](https://www.tutorialrepublic.com/javascript-tutorial/javascript-date-object.php)