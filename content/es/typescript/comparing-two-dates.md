---
title:    "TypeScript: Comparando dos fechas"
keywords: ["TypeScript"]
---

{{< edit_this_page >}}

## Por Qué

A menudo, en la programación, necesitamos comparar dos fechas. Ya sea para validar una entrada del usuario o para determinar la antigüedad de una publicación en un blog. Pero, ¿por qué deberíamos aprender a comparar fechas en TypeScript? ¡La respuesta es simple! Las comparaciones de fechas son fundamentales para el correcto funcionamiento de nuestras aplicaciones y pueden ayudarnos a tomar decisiones lógicas en nuestro código.

## Cómo

Comparar fechas en TypeScript es bastante sencillo. Primero, necesitamos crear dos objetos Date. Esto se puede hacer utilizando el constructor de Date y especificando una fecha en formato ISO o utilizando métodos como Date.now(). Una vez que tenemos nuestros dos objetos Date, podemos utilizar el operador mayor que (>) o menor que (<) para compararlos. Por ejemplo:

```TypeScript
let fecha1: Date = new Date("2021-01-20");
let fecha2: Date = new Date("2021-01-25");

if(fecha1 > fecha2) {
console.log("La fecha 1 es mayor que la fecha 2");
} else if (fecha1 < fecha2) {
console.log("La fecha 1 es menor que la fecha 2");
} else {
console.log("Las fechas son iguales");
}
```

En este ejemplo, si ejecutamos el código, obtendremos la siguiente salida:

```
La fecha 1 es menor que la fecha 2
```

Además de los operadores mayor y menor, también podemos utilizar otros métodos para comparar fechas en TypeScript, como getTime() para obtener el tiempo en milisegundos y luego compararlos utilizando los operadores antes mencionados.

## Deep Dive

Ahora que sabemos cómo comparar fechas en TypeScript, es importante tener en cuenta algunos detalles adicionales. Primero, al comparar objetos Date, se está comparando su valor de tiempo, no sus propiedades individuales como día, mes o año. Esto significa que dos objetos Date con valores de tiempo diferentes pueden ser iguales si representan la misma fecha. Además, cuando comparamos objetos Date utilizando los operadores > o <, estamos realizando una comparación de tipo estricto, lo que significa que también se compara el tipo de objeto. Por lo tanto, dos fechas con el mismo valor de tiempo pero creadas de manera diferente, no se considerarán iguales en la comparación. Por último, también es importante recordar que los objetos Date son mutables, por lo que es posible alterar su valor de tiempo utilizando métodos como setDate() o setFullYear(). Esto puede afectar la comparación de fechas y causar resultados inesperados.

## Ver también

- [Documentación oficial de TypeScript - Comparando fechas](https://www.typescriptlang.org/docs/handbook/standard-library.html#comparing-dates)
- [Comparar fechas en JavaScript](https://www.w3schools.com/js/js_dates_compare.asp)
- [Operadores de comparación en TypeScript](https://www.javatpoint.com/typescript-comparison-and-logical-operators)