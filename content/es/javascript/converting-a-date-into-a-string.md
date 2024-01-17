---
title:                "Convirtiendo una fecha en una cadena"
html_title:           "Javascript: Convirtiendo una fecha en una cadena"
simple_title:         "Convirtiendo una fecha en una cadena"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/javascript/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## ¿Qué y por qué?

Convertir una fecha en una cadena de texto es un proceso común en la programación moderna. Se refiere a la conversión de un objeto de fecha en un formato legible para los humanos, como por ejemplo "14 de abril de 2021". Los programadores hacen esto para mostrar fechas en un formato más fácil de entender para los usuarios y para realizar operaciones y cálculos con fechas en su código.

## Cómo hacerlo:

```Javascript
let fecha = new Date();
console.log(fecha.toString());
// Salida: "Wed Apr 14 2021 12:00:00 GMT-0400 (hora de verano oriental)"

let fecha2 = new Date('12/25/2021');
console.log(fecha2.toDateString());
// Salida: "Sat Dec 25 2021"
```

## Inmersión profunda:

Convertir fechas en cadenas de texto ha sido una necesidad en la programación desde los primeros días. Antes de que existieran los objetos de fecha en JavaScript, los programadores tenían que codificar manualmente la conversión de fechas en cadenas de texto. Hoy en día, también existen bibliotecas y módulos que facilitan esta tarea, como Moment.js y date-fns.

## Ver también:

- [Documentación de Date en MDN](https://developer.mozilla.org/es/docs/Web/JavaScript/Reference/Global_Objects/Date)
- [Moment.js - biblioteca para manejar fechas en JavaScript](https://momentjs.com/)
- [date-fns - kit de herramientas moderno para fechas en JavaScript](https://date-fns.org/)