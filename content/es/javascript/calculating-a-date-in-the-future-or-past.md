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

## ¿Qué y Por qué?
La programación es una herramienta muy versátil que nos permite realizar una variedad de tareas, incluso algo tan específico como calcular una fecha en el futuro o en el pasado. Los programadores a menudo hacen esto para automatizar procesos o para mostrar información relevante en sus aplicaciones.

## Como:
```Javascript
// Para calcular una fecha en el futuro, podemos usar el método "getDate()" en un objeto Date (Fecha) y pasarle el número de días que queremos agregar:
let fechaActual = new Date();
let diasAdicionales = 7;
let fechaFutura = fechaActual.getDate() + diasAdicionales;
console.log(fechaFutura); // Salida: 7
```

```Javascript
// También podemos calcular una fecha en el pasado, restándole días al valor inicial de la fecha:
let fechaActual = new Date();
let diasRestados = 3;
let fechaPasada = fechaActual.getDate() - diasRestados;
console.log(fechaPasada); // Salida: 29
```

## Profundizando:
La idea de calcular fechas en el futuro o en el pasado no es nueva, ya que ha sido una tarea común en la programación desde el principio. Sin embargo, con la implementación del objeto Date en JavaScript, se ha vuelto más fácil y eficiente. Además, existen librerías y módulos que pueden ayudar a los programadores a realizar cálculos más complejos de fechas, como Moment.js y date-fns.

## Ver también:
- Documentación de Date en MDN: https://developer.mozilla.org/es/docs/Web/JavaScript/Referencia/Objetos_globales/Date
- Moment.js: https://momentjs.com/
- date-fns: https://date-fns.org/