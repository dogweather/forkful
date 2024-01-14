---
title:    "Javascript: Calculando una fecha en el futuro o pasado"
keywords: ["Javascript"]
---

{{< edit_this_page >}}

## Por qué
 
Calcular fechas en el futuro o pasado es una tarea muy común tanto en el desarrollo de aplicaciones como en el manejo de datos. Puede ser utilizado en situaciones como programación de recordatorios, planificación de viajes o análisis de datos históricos. En este artículo, aprenderemos cómo calcular una fecha en el pasado o en el futuro utilizando Javascript.

## Cómo
 
Para calcular una fecha en el futuro o pasado en Javascript, necesitamos utilizar el objeto `Date` y sus respectivos métodos. Primero, debemos crear una instancia del objeto `Date` con la fecha actual utilizando `new Date()`. Luego, podemos utilizar los métodos `getFullYear()`, `getMonth()` y `getDate()` para obtener el año, mes y día de la fecha actual respectivamente. Estos métodos nos devuelven un número, por lo que debemos almacenarlos en variables. Por ejemplo:

````javascript
let hoy = new Date();
let anio = hoy.getFullYear();
let mes = hoy.getMonth();
let dia = hoy.getDate();
````
 
Una vez que tengamos estos valores, podemos utilizar el método `setFullYear()` para establecer una nueva fecha en el futuro o pasado, pasando como argumento el año deseado. También podemos utilizar los métodos `setMonth()` y `setDate()` para establecer el mes y día deseados. Por ejemplo:

````javascript
hoy.setFullYear(anio + 1); // Establece la fecha en 1 año en el futuro
hoy.setMonth(mes - 2); // Establece la fecha en 2 meses en el pasado
hoy.setDate(dia + 10); // Establece la fecha en 10 días en el futuro
````
 
Finalmente, para obtener la nueva fecha calculada, podemos utilizar el método `toLocaleDateString()` y pasar el idioma deseado como argumento. Este método nos devolverá la fecha en el formato adecuado para el idioma seleccionado. Por ejemplo:

````javascript
console.log(hoy.toLocaleDateString("es")); // Imprime la fecha en formato DD/MM/YYYY para el idioma español.
````
 
## Profundizando
 
La clase `Date` en Javascript también posee otros métodos útiles para el manejo de fechas, como `setHours()`, `setMinutes()`, `setSeconds()`, entre otros. También podemos utilizar el objeto `Intl.DateTimeFormat` para formatear fechas en idiomas específicos. Para más información sobre estos métodos, puedes consultar la documentación oficial de Javascript.
 
## Ver también
 
- [Documentación oficial de Javascript](https://developer.mozilla.org/es/docs/Web/JavaScript/Reference/Global_Objects/Date)
- [Objeto Intl.DateTimeFormat en Javascript](https://developer.mozilla.org/es/docs/Web/JavaScript/Reference/Global_Objects/Intl/DateTimeFormat)