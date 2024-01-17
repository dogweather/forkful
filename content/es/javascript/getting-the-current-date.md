---
title:                "Obteniendo la fecha actual"
html_title:           "Javascript: Obteniendo la fecha actual"
simple_title:         "Obteniendo la fecha actual"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/javascript/getting-the-current-date.md"
---

{{< edit_this_page >}}

## ¿Qué y por qué?
Obtener la fecha actual en programación es una forma de obtener información sobre la fecha y hora actual del sistema en el que se está ejecutando el código. Los programadores pueden hacerlo para realizar tareas como mostrar la fecha actual en una interfaz de usuario o para realizar cálculos basados en la fecha actual.

## Cómo hacerlo:
Para obtener la fecha actual en Javascript, simplemente podemos utilizar el objeto `Date` incorporado y su método `getDate()`. Este método devuelve un número que representa el día del mes actual. Por ejemplo:

```Javascript
let date = new Date();
console.log(date.getDate()); // Output: 3 (si hoy es 3 de mayo)
```

También podemos utilizar otros métodos como `getFullYear()` para obtener el año actual, `getMonth()` para obtener el mes actual o `getDay()` para obtener el día de la semana.

## Profundizando:
Historicamente, obtener la fecha actual en programación era una tarea más compleja que involucraba conversiones y cálculos. Sin embargo, con la incorporación del objeto `Date` en los lenguajes de programación, ahora es más fácil. Además, existen bibliotecas y módulos que ofrecen funcionalidades más avanzadas para trabajar con fechas y horas.

## Ver también:
- [MDN web docs: Date](https://developer.mozilla.org/es/docs/Web/JavaScript/Reference/Global_Objects/Date)
- [Moment.js](https://momentjs.com/) - Una biblioteca popular para trabajar con fechas en Javascript.
- [Luxon](https://moment.github.io/luxon/) - Una alternativa a Moment.js.