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

## ¿Qué y por qué?
Calcular una fecha en el pasado o en el futuro es una tarea común para los programadores. Esto se refiere a encontrar una fecha específica que se encuentre antes o después de una fecha dada. Los programadores usan este tipo de cálculo para realizar una variedad de tareas, como programar eventos, mostrar la fecha de vencimiento de una tarea o crear un calendario dinámico.

## Cómo hacerlo:
Para calcular una fecha en el futuro o en el pasado en TypeScript, necesitarás usar la clase ```Date``` y sus métodos para manejar las fechas. A continuación se muestran algunos ejemplos de código en TypeScript para calcular una fecha en el pasado o en el futuro.

Para calcular una fecha en el futuro, puedes usar el método ```setDate()``` para establecer la fecha actual y luego usar el método ```getDate()``` para obtener la fecha deseada. Por ejemplo:

```
let fecha = new Date(); // fecha actual
fecha.setDate(fecha.getDate() + 7); // fecha en 7 días
console.log(fecha); // resultado: 2021-05-20T13:46:34.659Z
```

Para calcular una fecha en el pasado, puedes usar el método ```setDate()``` de manera similar, pero esta vez restar un número de días a la fecha actual. Por ejemplo:

```
let fecha = new Date(); // fecha actual
fecha.setDate(fecha.getDate() - 7); // fecha hace 7 días
console.log(fecha); // resultado: 2021-05-06T13:46:34.659Z
```

## Profundizando:
La manipulación de fechas ha sido un desafío histórico para los programadores, especialmente antes de la introducción de las clases ```Date``` y ```Time``` en lenguajes de programación como C y Java. Estos problemas se complicaban aún más debido a las diferentes formas en que se calculan las fechas en diferentes partes del mundo.

Existen alternativas a la clase ```Date``` en TypeScript, como la librería moment.js, que ofrece una serie de métodos para manipular y formatear fechas de manera más sencilla.

En cuanto a la implementación, es importante tener en cuenta que las fechas son en realidad valores numéricos que representan la cantidad de milisegundos desde el 1 de enero de 1970. Por lo tanto, al manipular fechas en TypeScript, en realidad se están realizando operaciones matemáticas en estos valores numéricos.

## Ver también:
- Artículo sobre la clase Date en la documentación oficial de TypeScript: https://www.typescriptlang.org/docs/handbook/release-notes/typescript-2-2.html#support-for-strict-datetime-objects
- Moment.js: https://momentjs.com/