---
title:    "Javascript: Comparando dos fechas"
keywords: ["Javascript"]
---

{{< edit_this_page >}}

## Por qué comparar dos fechas en JavaScript

A menudo, cuando estamos trabajando con fechas en nuestras aplicaciones JavaScript, nos encontramos con la necesidad de comparar dos fechas diferentes. Esto puede ser útil para determinar si una fecha es anterior o posterior a otra, o si ambas fechas son iguales. En este artículo, exploraremos por qué es importante saber cómo comparar dos fechas y cómo hacerlo en Javascript de manera efectiva.

## Cómo comparar dos fechas en JavaScript

Para comparar dos fechas en JavaScript, podemos utilizar el operador de comparación `>` para verificar si una fecha es anterior a otra, `<` para verificar si es posterior y `===` para comprobar si son iguales. Sin embargo, cuando se trata de fechas, a menudo es necesario convertirlas primero a un formato más manejable para poder realizar la comparación de manera adecuada.

Por ejemplo, si tenemos dos fechas en formato de cadena o en objetos de tipo `Date`, primero tendremos que convertirlas a objetos de tipo `Date` y luego compararlas. Veamos un ejemplo de cómo hacerlo:

```javascript
let date1 = "2021-01-01";
let date2 = new Date("2021-01-05");

// Convertir fechas a objetos Date
let convertedDate1 = new Date(date1);
let convertedDate2 = new Date(date2);

// Comparar fechas
if (convertedDate1 < convertedDate2) {
  console.log("date1 es anterior a date2");
} else if (convertedDate1 > convertedDate2) {
  console.log("date1 es posterior a date2");
} else if (convertedDate1 === convertedDate2) {
  console.log("date1 es igual a date2");
}
```

En este ejemplo, utilizamos el método `new Date()` para convertir nuestras fechas a objetos de tipo `Date` y luego utilizamos el operador de comparación adecuado para realizar la comparación.

También podemos utilizar métodos propios de los objetos `Date`, como `getTime()` para obtener el valor en milisegundos de una fecha y luego comparar estos valores. Por ejemplo:

```javascript
if (date1.getTime() > date2.getTime()) {
  console.log("date1 es posterior a date2");
}
```

## Profundizando en la comparación de fechas en JavaScript

Aunque hemos visto cómo comparar fechas en su formato más común, es importante tener en cuenta que hay casos en los que debemos considerar otros aspectos, como la diferencia de zona horaria o la precisión de la comparación. En estos casos, podemos utilizar librerías externas como Moment.js que nos ofrecen más funcionalidades y opciones para comparar fechas en JavaScript.

Además, también es importante tener en cuenta que las fechas en JavaScript están limitadas al rango de fechas de enero de 1970 a diciembre de 9999, lo que puede afectar la precisión de algunas comparaciones.

## Ver también

- [Documentation on Date objects in JavaScript](https://developer.mozilla.org/pt-BR/docs/Web/JavaScript/Reference/Global_Objects/Date)
- [Moment.js Documentation](https://momentjs.com/docs/)
- [Comparar fechas en JavaScript con Moment.js](https://www.digitalocean.com/community/tutorials/comparar-fechas-javascript-moment-js)