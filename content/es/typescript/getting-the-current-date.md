---
title:                "Obteniendo la fecha actual"
html_title:           "C#: Obteniendo la fecha actual"
simple_title:         "Obteniendo la fecha actual"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/typescript/getting-the-current-date.md"
---

{{< edit_this_page >}}

# Cómo obtener la fecha actual en TypeScript

## ¿Qué y Por qué?

Obtener la fecha actual es simplemente un acto de recuperar la fecha y hora exactas en el momento en que se realiza la solicitud. Los programadores suelen hacer esto para realizar una amplia gama de tareas, como marcar la hora de eventos, registrar datos en tiempo real o incluso para funciones de temporización.

## ¿Cómo hacerlo?

Utilizaremos el objeto `Date` incorporado en JavaScript/TypeScript. Los siguientes son algunos métodos con los que puedes experimentar:

```TypeScript
let fechaActual = new Date();
console.log(fechaActual); // Muestra la fecha y hora actuales

let dia = fechaActual.getDate();
console.log(dia); // Muestra día del mes (1-31)

let mes = fechaActual.getMonth();
console.log(mes); // Muestra mes (0-11, 0 es Enero)

let año = fechaActual.getFullYear();
console.log(año); // Muestra el año en 4 dígitos

let hora = fechaActual.getHours();
console.log(hora); // Muestra la hora (0-23)
```

## Profundización

Historia: `Date` es un objeto incorporado en JavaScript desde su primera versión, por lo que ha existido desde el principio.

Alternativas: Existen bibliotecas de terceros como Moment.js y Date-fns que ofrecen funcionalidades más detalladas y versátiles en comparación con el objeto `Date` incorporado. 

Implementación: En TypeScript/JavaScript, `Date` trabaja con los valores de fecha y hora internamente en términos de milisegundos desde la "Época Unix", que se refiere a la medianoche UTC del 1 de enero de 1970.

## Ver También 

- Documentación oficial de `Date`: https://developer.mozilla.org/es/docs/Web/JavaScript/Referencia/Objetos_globales/Date
- Moment.js: https://momentjs.com/
- Date-fns: https://date-fns.org/