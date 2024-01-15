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

## Por Qué 

Convertir una fecha en una cadena de texto es una tarea común en JavaScript. Esto se debe a que las fechas son almacenadas en un formato específico de objetos y a menudo necesitamos presentarlas de una manera más comprensible para los usuarios. 

## Cómo Hacerlo 

Podemos convertir una fecha en una cadena de texto utilizando tres métodos principales: `toString()`, `toDateString()` y `toLocaleDateString()`. Veamos un ejemplo de cada uno de ellos: 

```Javascript 
// Crear una nueva fecha con la fecha actual 
let fecha = new Date(); 

// Utilizar el método toString() para convertirla en una cadena de texto 
console.log(fecha.toString()); // Ejemplo de salida: "Thu Oct 21 2021 14:22:04 GMT-0400 (Eastern Daylight Time)" 

// Utilizar el método toDateString() para obtener solo la parte de la fecha de la cadena de texto 
console.log(fecha.toDateString()); // Ejemplo de salida: "Thu Oct 21 2021" 

// Utilizar el método toLocaleDateString() para obtener la parte de la fecha en un formato localizado 
console.log(fecha.toLocaleDateString()); // Ejemplo de salida: "10/21/2021" 
``` 

Como se puede ver en el ejemplo, el método `toString()` devuelve la fecha y hora completa en una cadena de texto, mientras que los métodos `toDateString()` y `toLocaleDateString()` devuelven solo la parte de la fecha en diferentes formatos. 

## Deep Dive 

Es importante tener en cuenta que estos métodos son dependientes del navegador y pueden producir resultados diferentes en diferentes navegadores y sistemas operativos. También pueden ser afectados por la configuración regional del usuario. 

Para una mayor flexibilidad, también podemos utilizar el método `toISOString()`, que devuelve la fecha y hora en formato ISO. Podemos luego manipular esta cadena de texto para presentarla de la forma que queramos. Además, podemos utilizar librerías externas como Moment.js para una mayor personalización de la conversión de fechas en cadenas de texto. 

## Ver También 

- [Documentación de MDN sobre Date](https://developer.mozilla.org/es/docs/Web/JavaScript/Reference/Global_Objects/Date) 
- [Moment.js](https://momentjs.com/) 
- [Convertir una cadena de texto en fecha en JavaScript](https://www.toptal.com/software/definitive-guide-to-datetime-manipulation)