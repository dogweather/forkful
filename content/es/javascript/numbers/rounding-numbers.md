---
date: 2024-01-26 03:46:00.448057-07:00
description: "Redondear es eliminar el ruido despu\xE9s de un cierto punto en un n\xFA\
  mero. Los programadores redondean para controlar la precisi\xF3n, gestionar la memoria\
  \ o\u2026"
lastmod: '2024-03-13T22:44:59.453725-06:00'
model: gpt-4-0125-preview
summary: "Redondear es eliminar el ruido despu\xE9s de un cierto punto en un n\xFA\
  mero. Los programadores redondean para controlar la precisi\xF3n, gestionar la memoria\
  \ o\u2026"
title: "Redondeo de n\xFAmeros"
weight: 13
---

## Qué y Por Qué?
Redondear es eliminar el ruido después de un cierto punto en un número. Los programadores redondean para controlar la precisión, gestionar la memoria o hacer que la salida sea amigable para el usuario—como convertir 2.998 en un limpio 3.

## Cómo hacerlo:
Aquí te mostramos cómo redondear números en JavaScript usando `Math.round()`, `Math.ceil()`, y `Math.floor()`: 

```javascript
let originalNumber = 2.567;

let roundedDown = Math.floor(originalNumber); // 2
let roundedUp = Math.ceil(originalNumber);    // 3
let rounded = Math.round(originalNumber);     // 3 (ya que .567 es más que .5)

console.log(roundedDown); // Imprime: 2
console.log(roundedUp);   // Imprime: 3
console.log(rounded);     // Imprime: 3
```

Para fijar a un cierto número de decimales, usa `toFixed()`:

```javascript
let twoDecimals = originalNumber.toFixed(2); // "2.57" (devuelve un string)

console.log(twoDecimals); // Imprime: "2.57"
```

Convierte el string de vuelta a un número con un más unario o `Number()`:

```javascript
let numberAgain = +twoDecimals; // 2.57

console.log(numberAgain); // Imprime: 2.57
```

## Análisis Profundo
Redondear números no es algo nuevo; es tan antiguo como los números. En JavaScript, `Math.round()` utiliza el método de "redondear hacia arriba la mitad" para desempatar: si la parte fraccional es 0.5, redondea al número par más cercano.

Para tener más control, `toFixed()` podría ser tu mejor opción, pero recuerda, devuelve un string. Convertirlo de nuevo a número puede ser un paso extra pero asegura que sigues trabajando con tipos numéricos.

¿Alternativas? Bibliotecas como `lodash` ofrecen `_.round(number, [precision=0])` para un control más matizado. O, el más nuevo `Intl.NumberFormat` te ofrece formateo de alta precisión más allá del simple redondeo.

Hablando de precisión, ten cuidado con las peculiaridades de los puntos flotantes en JavaScript. `0.1 + 0.2` no es exactamente igual a `0.3` debido a cómo se almacenan los números. A veces, redondear se hace necesario para corregir estos errores de punto flotante.

## Ver También
- Documentación de Math de Mozilla: [MDN Web Docs](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Math)
- Redondeo financiero con `Intl.NumberFormat`: [API de Internacionalización ECMAScript](https://tc39.es/ecma402/#numberformat-objects)
- Redondeo con `lodash`: [Documentos de Lodash](https://lodash.com/docs/4.17.15#round)
