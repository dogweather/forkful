---
title:                "Javascript: Obteniendo la fecha actual"
programming_language: "Javascript"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/javascript/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Por qué

En la programación de JavaScript, es común necesitar obtener la fecha actual. Ya sea para mostrarla en una página web, registrar el tiempo de una acción o realizar cualquier otra tarea, conocer la fecha actual es fundamental. Afortunadamente, JavaScript cuenta con una herramienta integrada para obtener la fecha y hora actual fácilmente. En esta publicación, aprenderemos cómo obtener la fecha actual en JavaScript y cómo podemos manipularla para adaptarla a nuestras necesidades.

## Cómo hacerlo

Para obtener la fecha actual en JavaScript, utilizamos el objeto incorporado Date. Este objeto contiene métodos para obtener la fecha, la hora y otros detalles relacionados con el tiempo. Para obtener la fecha actual, simplemente usamos el método `getDate()` junto con `new Date()`, de la siguiente manera:

```
let fechaActual = new Date();
let dia = fechaActual.getDate();
```

En este código, creamos una variable `fechaActual` que contiene el objeto Date con la fecha y hora actuales. Luego, utilizamos el método `getDate()` para obtener el día del mes actual y lo almacenamos en la variable `dia`. Podemos acceder a otros valores como el mes, el año o la hora utilizando métodos similares (e.g. `getMonth()`, `getFullYear()`, `getHours()`).

También podemos personalizar el formato de la fecha utilizando el método `toLocaleDateString()` y pasando como parámetros las opciones de idioma y formato que deseamos. Por ejemplo:

```
let fechaActual = new Date();
let opciones = { weekday: 'long', year: 'numeric', month: 'long', day: 'numeric' };
fechaActual.toLocaleDateString('es-ES', opciones); // Viernes, 19 de marzo de 2021
```

En este caso, especificamos que queremos la fecha en formato largo de España y nos devuelve la fecha en el formato deseado.

## Profundizando

El objeto Date en JavaScript también permite manipular y comparar fechas. Por ejemplo, podemos crear una fecha específica utilizando el constructor con parámetros (año, mes, día) y luego compararla con la fecha actual utilizando operadores de comparación como `>`, `<`, `>=`, `<=`.

Además, el objeto Date también cuenta con métodos para sumar o restar días, meses o años a una fecha específica y obtener una nueva fecha resultante. Esto puede ser útil para tareas como calcular una fecha de vencimiento o una fecha futura para un recordatorio.

## Ver también

- [Objeto Date en MDN](https://developer.mozilla.org/es/docs/Web/JavaScript/Reference/Global_Objects/Date)
- [Manipulación de fechas en JavaScript](https://www.w3schools.com/js/js_dates.asp)
- [Formato de fechas en JavaScript](https://www.w3schools.com/jsref/jsref_tolocaletimestring.asp)