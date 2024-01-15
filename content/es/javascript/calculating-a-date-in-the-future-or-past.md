---
title:                "Calculando una fecha en el futuro o pasado"
html_title:           "Javascript: Calculando una fecha en el futuro o pasado"
simple_title:         "Calculando una fecha en el futuro o pasado"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/javascript/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## ¿Por qué calcular una fecha en el futuro o en el pasado?

Calcular fechas en el futuro o en el pasado puede ser útil en muchas situaciones, como programación de eventos, cálculo de vencimientos o incluso en tareas de planificación personal. Poder realizar este cálculo de manera eficiente y precisa es una habilidad importante para cualquier programador de Javascript.

## Cómo hacerlo:
Para calcular una fecha en el futuro o en el pasado en Javascript, se pueden utilizar las propiedades y métodos de la clase ```Date```. Primero, se debe crear una instancia de esta clase con la fecha a partir de la cual se desea calcular la fecha futura o pasada.

```Javascript
let fechaActual = new Date(); // Crear instancia de la clase Date con la fecha actual
```

Luego, se pueden utilizar los métodos ```setFullYear()```, ```setMonth()``` y ```setDate()``` para establecer la fecha en la que se desea calcular la fecha futura o pasada. Estos métodos reciben como parámetros el año, el mes y el día respectivamente.

```Javascript
fechaActual.setFullYear(2021); // Establecer año (2021)
fechaActual.setMonth(10); // Establecer mes (noviembre)
fechaActual.setDate(15); // Establecer día (15)
```

Una vez establecida la fecha deseada, se puede utilizar el método ```getTime()``` para obtener el timestamp correspondiente a esa fecha. Luego, se puede utilizar una simple operación matemática para sumar o restar la cantidad de milisegundos correspondientes al intervalo de tiempo deseado.

```Javascript
let tiempoActual = fechaActual.getTime(); // Obtener timestamp correspondiente a la fecha actual
let tiempoFuturo = tiempoActual + (86400000 * 7); // Sumar 7 días (86400000 milisegundos equivalen a un día)
let tiempoPasado = tiempoActual - (86400000 * 30); // Restar 30 días (un mes)
```

Finalmente, se puede convertir el timestamp obtenido nuevamente a una fecha utilizando el método ```setTime()```.

```Javascript
fechaActual.setTime(tiempoFuturo); // Convertir timestamp a fecha
console.log(fechaActual); // Output: Mon Nov 22 2021 22:20:00 GMT-0500 (hora estándar de Colombia)
```

## Profundizando:
Para entender mejor cómo funcionan los cálculos de fechas en JavaScript, es importante tener en cuenta que los timestamps se basan en el 1 de enero de 1970 a las 00:00:00 UTC. Esto significa que cualquier operación realizada en un timestamp tendrá en cuenta esta fecha de referencia.

Además, es importante tener en cuenta los diferentes formatos de fechas en JavaScript, ya que pueden variar según la región o la zona horaria. Por ejemplo, mientras que en América del Norte el formato por defecto es MM/DD/YYYY, en Europa es DD/MM/YYYY. Por lo tanto, es importante verificar la forma en que se están ingresando y manejando las fechas en el código.

Otra cosa a tener en cuenta es que al sumar o restar días a una fecha, se puede obtener un resultado inesperado debido a la existencia de horarios de verano y horarios de invierno en diferentes zonas horarias.

En conclusión, realizar cálculos de fechas en Javascript es una habilidad importante para un programador, pero requiere un buen entendimiento de cómo funcionan los timestamps y los diferentes formatos y zonas horarias. Con la comprensión adecuada, se pueden realizar cálculos de fechas precisos y útiles en cualquier proyecto.

## Ver también:
- [Documentación de la clase Date en MDN](https://developer.mozilla.org/es/docs/Web/JavaScript/Referencia/Objetos_globales/Date)
- [Guía para trabajar con fechas en Javascript](https://www.w3schools.com/js/js_dates.asp)
- [Formatos de fechas y horas en Javascript](https://www.date-hora.com/javascript-tutorial/fecha-hora-javascript.html)