---
date: 2024-01-26 03:46:56.869208-07:00
description: "C\xF3mo hacerlo: El redondeo en TypeScript se puede hacer utilizando\
  \ varios m\xE9todos. Aqu\xED hay un r\xE1pido repaso."
lastmod: '2024-03-13T22:44:58.794829-06:00'
model: gpt-4-0125-preview
summary: "El redondeo en TypeScript se puede hacer utilizando varios m\xE9todos."
title: "Redondeo de n\xFAmeros"
weight: 13
---

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
