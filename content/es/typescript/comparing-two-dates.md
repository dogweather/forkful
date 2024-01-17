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

## Qué y por qué?
Comparar dos fechas en TypeScript es una forma de determinar si una fecha es anterior, posterior o igual a otra fecha. Los programadores usan esta función para realizar operaciones lógicas y tomar decisiones basadas en la relación entre dos fechas.

## Cómo:
```TypeScript
// Crear dos fechas
let fecha1 = new Date("2021-01-01");
let fecha2 = new Date("2020-01-01");

// Comparar si fecha1 es anterior a fecha2
if (fecha1 < fecha2) {
  console.log("fecha1 es anterior a fecha2");
}

// Comparar si fecha1 es posterior a fecha2
if (fecha1 > fecha2) {
  console.log("fecha1 es posterior a fecha2");
}

// Comparar si fecha1 es igual a fecha2
if (fecha1 === fecha2) {
  console.log("fecha1 es igual a fecha2");
}
```

### Salida:
```
fecha1 es posterior a fecha2
```

## Profundizando:
Comparar fechas ha sido una función importante en la programación desde los primeros días de los lenguajes de programación. En TypeScript, las fechas son objetos de tipo `Date` y podemos utilizar los operadores de comparación (`<`, `>`, `===`) para compararlas. Sin embargo, también existen librerías como `moment.js` o `date-fns` que ofrecen funciones más avanzadas para comparar fechas, como contar el número de días entre dos fechas o comprobar si una fecha está dentro de un rango.

## Ver también:
- [Documentación oficial de TypeScript sobre fechas](https://www.typescriptlang.org/docs/handbook/release-notes/typescript-2-2.html#example-1)
- [Documentación de la librería moment.js](https://momentjs.com/)
- [Documentación de la librería date-fns](https://date-fns.org/)