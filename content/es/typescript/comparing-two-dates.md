---
title:                "Comparando dos fechas"
html_title:           "TypeScript: Comparando dos fechas"
simple_title:         "Comparando dos fechas"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/typescript/comparing-two-dates.md"
---

{{< edit_this_page >}}

## ¿Por qué comparar dos fechas?

Comparar dos fechas es una tarea común en cualquier tipo de aplicación, ya sea para verificar si una fecha es anterior o posterior a otra, por ejemplo en formularios de reservas o pagos. Además, puede ser útil para realizar cálculos de tiempo y programar acciones basadas en fechas específicas.

## Cómo hacerlo en TypeScript

En TypeScript, podemos utilizar el objeto `Date` para representar una fecha en nuestro código. Para comparar dos fechas, podemos utilizar los métodos `getTime()` y `setTime()` para obtener y establecer el tiempo en milisegundos desde el 1 de enero de 1970.

Por ejemplo, si queremos comparar si una fecha es anterior a otra, podemos hacer lo siguiente:

```TypeScript
const fecha1 = new Date(2021, 9, 1); // 1 de octubre de 2021
const fecha2 = new Date(2021, 9, 15); // 15 de octubre de 2021

if (fecha1.getTime() < fecha2.getTime()) {
  console.log("fecha1 es anterior a fecha2");
}
```

En este caso, el método `getTime()` nos devuelve el tiempo en milisegundos desde 1970 para ambas fechas y luego los comparamos utilizando el operador `<`.

También podemos utilizar el método `getTime()` para calcular la diferencia entre dos fechas, simplemente realizando la operación en los tiempos obtenidos. Por ejemplo:

```TypeScript
const fecha1 = new Date(2021, 9, 1); // 1 de octubre de 2021
const fecha2 = new Date(2021, 9, 15); // 15 de octubre de 2021

const diferencia = fecha2.getTime() - fecha1.getTime();
console.log(`La diferencia entre las dos fechas es de ${diferencia / (1000 * 60 * 60 * 24)} días`);
```

En este ejemplo, también utilizamos el método `getTime()` para obtener el tiempo en milisegundos desde 1970 y luego realizamos la operación para convertirlo en días.

## Profundizando en la comparación de fechas

Además de utilizar el método `getTime()` para comparar fechas, también podemos utilizar otros métodos del objeto `Date` como `getDate()`, `getMonth()` y `getFullYear()` para obtener valores específicos de cada fecha y compararlos.

Por ejemplo, si queremos comparar si dos fechas tienen el mismo mes:

```TypeScript
const fecha1 = new Date(2021, 9, 1); // 1 de octubre de 2021
const fecha2 = new Date(2021, 9, 15); // 15 de octubre de 2021

if (fecha1.getMonth() === fecha2.getMonth()) {
  console.log("Ambas fechas tienen el mismo mes");
}
```

En este caso, utilizamos el método `getMonth()` para obtener el mes de cada fecha y luego comparamos los valores utilizando el operador `===`.

## Véase también

- [Documentación de TypeScript sobre el objeto Date](https://www.typescriptlang.org/docs/handbook/release-notes/typescript-2-2.html#object-spread-and-rest)
- [Comparar fechas en JavaScript](https://developer.mozilla.org/es/docs/Web/JavaScript/Referencia/Objetos_globales/Date/compare)
- [Ejemplos de comparación de fechas en TypeScript](https://www.techiediaries.com/typescript-date-compare/)