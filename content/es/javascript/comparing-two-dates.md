---
title:                "Comparando dos fechas"
html_title:           "Javascript: Comparando dos fechas"
simple_title:         "Comparando dos fechas"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/javascript/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Por quéComparar fechas es una tarea común en la programación, especialmente en aplicaciones que involucran eventos o tareas programadas. Al comparar dos fechas, podemos determinar qué evento sucedió primero, si dos eventos sucedieron el mismo día, o incluso calcular la diferencia de tiempo entre dos fechas.

## Cómo hacerlo

Para comparar dos fechas en JavaScript, podemos utilizar el objeto `Date` y sus métodos `getTime()` y `setTime()`. Estos métodos nos permiten convertir una fecha a un número en milisegundos, lo que facilita la comparación.

Por ejemplo, si queremos comparar dos fechas para determinar cuál sucedió primero, podemos hacerlo de la siguiente manera:

```
let fecha1 = new Date("January 1, 2021");
let fecha2 = new Date("January 15, 2021");

if (fecha1.getTime() < fecha2.getTime()) {
  console.log("La fecha 1 sucedió antes que la fecha 2");
} else if (fecha1.getTime() > fecha2.getTime()) {
  console.log("La fecha 2 sucedió antes que la fecha 1");
} else {
  console.log("Ambas fechas sucedieron el mismo día");
}

// Output: La fecha 1 sucedió antes que la fecha 2
```

También podemos comparar el año, mes y día de dos fechas utilizando los métodos `getFullYear()`, `getMonth()` y `getDate()` respectivamente. Por ejemplo, si queremos determinar si dos eventos ocurrieron en el mismo mes, podemos hacer lo siguiente:

```
let evento1 = new Date("February 10, 2021");
let evento2 = new Date("February 25, 2021");

if (evento1.getFullYear() == evento2.getFullYear() && evento1.getMonth() == evento2.getMonth()) {
  console.log("Ambos eventos ocurrieron en el mismo mes");
} else {
  console.log("Los eventos no ocurrieron en el mismo mes");
}

// Output: Ambos eventos ocurrieron en el mismo mes
```

## Inmersión profunda

Hay más métodos que podemos utilizar para comparar fechas en JavaScript, como `getHours()`, `getMinutes()` y `getSeconds()`, que nos permiten comparar la hora y el minuto de una fecha. También podemos utilizar el operador de comparación `===` para comparar directamente dos fechas en milisegundos.

Además, es importante tener en cuenta que al comparar dos fechas, debemos asegurarnos de usar el mismo formato de fecha para evitar resultados inesperados. Es recomendable convertir ambas fechas a un formato estándar para facilitar la comparación.

## Ver también

- Tutorial de JavaScript: Manipulación de fechas (https://www.w3schools.com/js/js_dates.asp)
- Documentación de MDN: Objeto Date (https://developer.mozilla.org/es/docs/Web/JavaScript/Referencia/Objetos_globales/Date)