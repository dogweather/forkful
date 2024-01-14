---
title:                "TypeScript: Calculando una fecha en el futuro o pasado"
simple_title:         "Calculando una fecha en el futuro o pasado"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/typescript/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Por qué

Calcular fechas en el futuro o en el pasado es una tarea común en la programación. Puede ser útil para planificar eventos, crear recordatorios o simplemente realizar cálculos de tiempo en una aplicación. En este artículo, exploraremos cómo calcular y manipular fechas en TypeScript.

## Cómo hacerlo

Para realizar cálculos de fechas en TypeScript, utilizaremos la clase `Date` nativa de JavaScript. Esta clase nos permite crear objetos de fecha y realizar operaciones con ellos. Comencemos con algunos ejemplos básicos:

```TypeScript
// Crear una nueva instancia de Date con la fecha y hora actual
const today = new Date();

// Crear una nueva instancia de Date con una fecha específica
const christmas = new Date(2021, 11, 25);

// Obtener el día de la semana de una fecha
const dayOfWeek = today.getDay();

// Obtener el mes de una fecha (los meses comienzan en 0, por lo que enero es el mes 0)
const month = christmas.getMonth();

// Comparar dos fechas para determinar cuál es más reciente
if (today > christmas) {
  console.log('Today is after Christmas!');
}
```

Estos son solo algunos ejemplos de lo que se puede hacer con la clase `Date`. También podemos realizar operaciones matemáticas con las fechas, como sumar y restar días:

```TypeScript
// Sumar 5 días a una fecha
const plusFiveDays = new Date(today.getTime() + (5*24*60*60*1000));

// Restar 2 semanas a una fecha
const minusTwoWeeks = new Date(today.getTime() - (14*24*60*60*1000));
```

También podemos formatear la fecha y hora de diferentes maneras, utilizando métodos como `toLocaleString()` y `toISOString()`. Para obtener más información sobre todas las posibilidades de la clase `Date`, consulte la [documentación oficial de TypeScript](https://www.typescriptlang.org/docs/handbook/declarations.html#built-in-types).

## Profundizando

Si bien hemos cubierto los conceptos básicos de cómo calcular fechas en TypeScript, existen muchas librerías y herramientas útiles que pueden ayudarnos en tareas más complejas. Algunas de estas incluyen Moment.js y date-fns, que ofrecen métodos adicionales para formatear, manipular y calcular fechas. También es importante entender cómo funcionan las zonas horarias y el horario de verano al realizar cálculos de fechas.

Es crucial tener en cuenta que el manejo de fechas puede ser problemático y propenso a errores en cualquier lenguaje de programación. Por lo tanto, es importante seguir buenas prácticas y validar cuidadosamente nuestras entradas y salidas de fechas.

## Ver también

- [Documentación oficial de TypeScript sobre la clase Date](https://www.typescriptlang.org/docs/handbook/declarations.html#built-in-types)
- [Moment.js](https://momentjs.com/)
- [date-fns](https://date-fns.org/)