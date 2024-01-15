---
title:                "Calculando una fecha en el futuro o pasado"
html_title:           "TypeScript: Calculando una fecha en el futuro o pasado"
simple_title:         "Calculando una fecha en el futuro o pasado"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/typescript/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## ¿Por qué calcular una fecha en el futuro o en el pasado?

Calcular fechas en el futuro o en el pasado es una tarea común en la programación. Puede ser útil para realizar tareas como programar eventos o notificaciones, generar informes basados ​​en fechas específicas, o simplemente mostrar la fecha correcta en diferentes zonas horarias.

## Cómo hacerlo

El tipo de dato `Date` de TypeScript nos permite crear y manipular fechas. Podemos usar este tipo de dato para calcular fechas en el futuro o en el pasado. Veamos algunos ejemplos:

Calculando una fecha en el futuro:
```TypeScript
let hoy = new Date(); // Creamos una fecha actual
let manana = new Date(hoy.getTime() + 1000 * 60 * 60 * 24); // Sumamos 1 día en milisegundos al tiempo actual
console.log(manana); // Output: 2021-08-20T13:57:50.985Z
```

Calculando una fecha en el pasado:
```TypeScript
let hoy = new Date(); // Creamos una fecha actual
let ayer = new Date(hoy.getTime() - 1000 * 60 * 60 * 24); // Restamos 1 día en milisegundos al tiempo actual
console.log(ayer); // Output: 2021-08-18T13:57:50.985Z
```

## Profundizando

Al calcular fechas en el futuro o en el pasado, es importante tener en cuenta el huso horario en el que se encuentra el usuario. Por ejemplo, si nuestro código se está ejecutando en una zona horaria diferente a la del usuario, puede haber un desfase en la fecha calculada.

Para evitar esto, podemos usar los métodos `getUTCDate()`, `getUTCMonth()` y `getUTCFullYear()` en lugar de los métodos `getDate()`, `getMonth()` y `getFullYear()`. Estos métodos nos devuelven la fecha y hora en formato UTC (Tiempo Universal Coordinado) en lugar del tiempo local.

También es importante tener en cuenta los cambios de horario de verano y de invierno, ya que pueden afectar la precisión de nuestras fechas calculadas.

## Ver también
- [Documentación oficial de TypeScript sobre el tipo de dato Date](https://www.typescriptlang.org/docs/handbook/release-notes/typescript-4-5.html#exponentiation-assignment-operators)
- [Calculating dates in the past or future in JavaScript](https://www.w3schools.com/js/js_date_methods.asp)
- [Working with time zones in TypeScript](https://www.pragimtech.com/blog/typescript/working-with-time-zones-in-typescript/)