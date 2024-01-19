---
title:                "Analizando una fecha desde una cadena de texto"
html_title:           "PHP: Analizando una fecha desde una cadena de texto"
simple_title:         "Analizando una fecha desde una cadena de texto"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/typescript/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

# Parseando fechas desde un string en TypeScript

## ¿Por qué y para qué?

Parsear una fecha desde un string consiste en tomar una representación textual de una fecha, como "21/12/2022", y convertirla en una representación de fecha/o tiempo entendible por una máquina, en este caso un objeto `Date` en JavaScript/TypeScript. Los programadores hacen esto cuando necesitan manejar fechas/tiempos en su código, o sea, para operaciones como comparar fechas, calcular la diferencia entre fechas, o formatear fechas de maneras específicas.

## ¿Cómo se hace?

La manera más sencilla y directa de parsear una fecha desde un string en TypeScript es usando el constructor de la clase `Date`:

```TypeScript
let dateStr = "2022-12-21";
let dateObj = new Date(dateStr);
```

Y puedes comprobarlo fácilmente con:

```TypeScript
console.log(dateObj);  // Salida: 2022-12-21T00:00:00.000Z
```

## Profundizando

En cuanto a los detalles de implementación, JavaScript y TypeScript utilizan el motor de fecha y tiempo de ECMAScript, que asume que todas las fechas y horas son UTC a menos que se indique lo contrario.

En términos de alternativas, podrías usar librerías como [moment.js](https://momentjs.com/) o [date-fns](https://date-fns.org/) que te proporcionan mayor flexibilidad y funciones para trabajar con fechas y tiempos.

Históricamente, la manipulación de fechas y tiempo ha sido una fuente de errores a medida que los programas se vuelven más globalizados y necesitan manejar zonas horarias, horario de verano, etc. Por esta razón, siempre es una buena idea usar una librería bien mantenida en lugar de intentar manejar las fechas y tiempos por ti mismo.

## Ver también

Si quieres profundizar más en este tema puede revisar los siguientes enlaces:

* [MDN - Date](https://developer.mozilla.org/es/docs/Web/JavaScript/Reference/Global_Objects/Date)
* [TypeScript - Date](https://www.tutorialsteacher.com/typescript/typescript-date)
* [moment.js](https://momentjs.com/)
* [date-fns](https://date-fns.org/)