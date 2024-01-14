---
title:                "Javascript: Convirtiendo una fecha en un texto"
programming_language: "Javascript"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/javascript/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Por qué
En la programación, a menudo es necesario trabajar con fechas y horas. Sin embargo, a menudo estas son almacenadas en un formato de objeto de fecha, lo que puede ser difícil de leer para los usuarios. Convertir una fecha en una cadena de texto puede hacer que sea más fácil de entender y manejar para los usuarios.

## Cómo hacerlo
Para convertir una fecha en una cadena de texto, hay dos métodos que se pueden utilizar: `toDateString()` y `toLocaleDateString()`. Ambos métodos tienen ligeras diferencias en la forma en que formatean la fecha, por lo que es importante elegir el más adecuado para tus necesidades.

A continuación se muestra un ejemplo de cómo convertir una fecha en una cadena de texto utilizando ambos métodos:

```Javascript
let currentDate = new Date(); // Hoy es Sat Sep 04 2021 10:14:13 GMT-0400 (hora de verano oriental)
console.log(currentDate.toDateString()); // muestra "Sat Sep 04 2021"
console.log(currentDate.toLocaleDateString()); // muestra "09/04/2021"
```

Como se puede ver en el ejemplo, `toDateString()` devuelve la fecha en un formato más legible para los humanos, mientras que `toLocaleDateString()` da la opción de especificar un idioma o localización específica para el formato de la fecha.

También es importante tener en cuenta que estos métodos solo funcionan con objetos de fecha válidos, por lo que si se intenta convertir una fecha inválida, se obtendrá un resultado de "Invalid Date".

## Profundizando
Además de los dos métodos mencionados anteriormente, también existe una biblioteca popular llamada moment.js que ofrece una gama aún más amplia de opciones para formatear fechas en cadenas de texto. Esta biblioteca también maneja fechas inválidas y da la opción de especificar formatos personalizados para la fecha.

Otra cosa a tener en cuenta es que las fechas en JavaScript se muestran en función de la zona horaria del dispositivo en el que se está ejecutando el código. Para asegurarse de que se muestren las fechas correctas en diferentes zonas horarias, se pueden utilizar métodos para obtener el desfase horario actual y ajustar la fecha en consecuencia.

En resumen, convertir una fecha en una cadena de texto puede facilitar su lectura y manipulación para los usuarios. Hay varias opciones a considerar, desde los métodos incorporados hasta bibliotecas externas, dependiendo de las necesidades específicas del proyecto.

## Ver también
- Documentación de toDateString(): https://developer.mozilla.org/es/docs/Web/JavaScript/Referencia/Objetos_globales/Date/toDateString
- Documentación de toLocaleDateString(): https://developer.mozilla.org/es/docs/Web/JavaScript/Referencia/Objetos_globales/Date/toLocaleDateString
- Moment.js: https://momentjs.com/