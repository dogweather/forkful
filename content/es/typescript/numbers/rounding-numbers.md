---
aliases:
- /es/typescript/rounding-numbers/
date: 2024-01-26 03:46:56.869208-07:00
description: "Redondear n\xFAmeros implica ajustar un n\xFAmero a una precisi\xF3\
  n espec\xEDfica. Los programadores lo hacen para controlar la salida num\xE9rica\
  \ para mejorar la\u2026"
lastmod: 2024-02-18 23:09:09.697414
model: gpt-4-0125-preview
summary: "Redondear n\xFAmeros implica ajustar un n\xFAmero a una precisi\xF3n espec\xED\
  fica. Los programadores lo hacen para controlar la salida num\xE9rica para mejorar\
  \ la\u2026"
title: "Redondeo de n\xFAmeros"
---

{{< edit_this_page >}}

## Qué y Por Qué?
Redondear números implica ajustar un número a una precisión específica. Los programadores lo hacen para controlar la salida numérica para mejorar la legibilidad, fines de visualización o cuando se requiere una precisión específica después de operaciones que generan resultados de punto flotante.

## Cómo hacerlo:
El redondeo en TypeScript se puede hacer utilizando varios métodos. Aquí hay un rápido repaso:

```typescript
// Math.round redondea al entero más cercano
console.log(Math.round(1.5)); // Salida: 2

// Math.ceil redondea hacia arriba al entero más cercano
console.log(Math.ceil(1.1)); // Salida: 2

// Math.floor redondea hacia abajo al entero más cercano
console.log(Math.floor(1.8)); // Salida: 1

// toFixed redondea a un número fijo de decimales
let num = 1.23456;
console.log(num.toFixed(2)); // Salida: "1.23"
// Nota: toFixed devuelve una cadena! Utiliza parseFloat para convertir de nuevo si es necesario.
console.log(parseFloat(num.toFixed(2))); // Salida: 1.23
```

## Análisis Profundo
En el pasado, el redondeo era necesario debido al espacio limitado y los problemas de precisión en las primeras computadoras. Hoy en día, la aritmética de punto flotante puede conducir a resultados peculiares debido a cómo se almacenan los números en binario. Alternativas al redondeo incluyen floor, ceil y trunc (para cortar decimales sin redondear).

Es digno de nota los internos: `Math.round` sigue el "redondeo hacia arriba" (también conocido como "redondeo comercial"), mientras que `Math.floor` y `Math.ceil` son directos. `toFixed` puede causar resultados inesperados porque devuelve una cadena, y redondea usando "redondeo a la mitad al par" (también conocido como "redondeo bancario"), especialmente útil para reducir el sesgo al redondear los mismos números varias veces.

## Ver También
- [MDN - Math.round()](https://developer.mozilla.org/es/docs/Web/JavaScript/Reference/Global_Objects/Math/round)
- [MDN - Math.ceil()](https://developer.mozilla.org/es/docs/Web/JavaScript/Reference/Global_Objects/Math/ceil)
- [MDN - Math.floor()](https://developer.mozilla.org/es/docs/Web/JavaScript/Reference/Global_Objects/Math/floor)
- [MDN - toFixed()](https://developer.mozilla.org/es/docs/Web/JavaScript/Reference/Global_Objects/Number/toFixed)
- [Estándar IEEE para Aritmética de Punto Flotante (IEEE 754)](https://ieeexplore.ieee.org/document/4610935)
