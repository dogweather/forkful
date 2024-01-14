---
title:                "Javascript: Comparando dos fechas"
simple_title:         "Comparando dos fechas"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/javascript/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Por qué
Hay muchas razones por las cuales uno podría querer comparar dos fechas en un programa. Por ejemplo, podría ser útil al trabajar con horarios o plazos de vencimiento, o para determinar la diferencia de tiempo entre dos eventos.

## Cómo hacerlo
Comparar dos fechas en JavaScript es bastante sencillo. Primero, necesitamos crear dos objetos de fecha utilizando la función `new Date()`. Podemos pasar una cadena de fecha como argumento para especificar una fecha específica, o simplemente dejarlo en blanco para obtener la fecha y hora actuales.

A continuación, podemos utilizar operadores de comparación, como `==`, `>`, o `<`, para comparar las dos fechas. Por ejemplo:
```Javascript
let fecha1 = new Date("June 10, 2021");
let fecha2 = new Date();

if (fecha1 > fecha2) {
  console.log("La fecha 1 es más reciente que la fecha 2");
} else if (fecha1 < fecha2) {
  console.log("La fecha 2 es más reciente que la fecha 1");
} else {
  console.log("Ambas fechas son iguales");
}
```

La salida de este código dependerá de la fecha actual en la que se ejecute.

También podemos utilizar los métodos `getTime()` y `setTime()` para obtener y establecer el tiempo en milisegundos de una fecha, lo que nos permite comparar fechas con una mayor precisión. Por ejemplo:
```Javascript
let fecha1 = new Date(2021, 5, 10);
let fecha2 = new Date(2021, 5, 11);

if (fecha2.getTime() > fecha1.getTime()) {
  console.log("La fecha 2 es un día más reciente que la fecha 1");
}
```

## Profundizando
Sin embargo, puede haber casos en los que simplemente utilizar operadores de comparación no sea suficiente. Por ejemplo, si queremos comparar solo las fechas y no tener en cuenta la hora. En este caso, podemos utilizar el método `setHours()` para establecer la hora a cero antes de comparar las fechas.

Otra cosa a tener en cuenta es que las fechas en JavaScript se manejan en el huso horario del navegador del usuario, por lo que los resultados pueden variar según la ubicación de la persona que interactúa con el programa. Para evitar esto, es importante especificar explícitamente el huso horario en las fechas que creamos.

## Ver también
- [Documentación de MDN sobre Date en JavaScript](https://developer.mozilla.org/es/docs/Web/JavaScript/Referencia/Objetos_globales/Date)
- [Artículo sobre la importancia de especificar huso horario en fechas en JavaScript](https://www.digitalocean.com/community/tutorials/datetime-formatting-using-javascript-date#getutcmethods)
- [Ejemplos de uso del objeto Date en JavaScript](https://www.w3schools.com/jsref/jsref_obj_date.asp)