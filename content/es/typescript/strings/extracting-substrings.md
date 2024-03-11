---
date: 2024-01-20 17:46:37.750659-07:00
description: "Extraer subcadenas significa seleccionar partes espec\xEDficas de una\
  \ cadena de texto. Programadores lo hacen para manipular, analizar o transformar\
  \ datos\u2026"
lastmod: '2024-03-11T00:14:32.607601-06:00'
model: gpt-4-1106-preview
summary: "Extraer subcadenas significa seleccionar partes espec\xEDficas de una cadena\
  \ de texto. Programadores lo hacen para manipular, analizar o transformar datos\u2026"
title: "Extracci\xF3n de subcadenas"
---

{{< edit_this_page >}}

## Qué y Por Qué?
Extraer subcadenas significa seleccionar partes específicas de una cadena de texto. Programadores lo hacen para manipular, analizar o transformar datos basados en texto, como extraer nombres de usuario de emails o valores de una URL.

## Cómo:
```TypeScript
let texto: string = "Hola, mundo maravilloso!";
let subcadena: string = texto.substring(7, 13); // "mundo"

console.log(subcadena); // Salida: mundo

// Utilizando slice para extraer desde el final
let subcadenaDesdeFinal: string = texto.slice(-10);
console.log(subcadenaDesdeFinal); // Salida: maravilloso!

// Otra manera con substr (obsoleto, pero aún presente en algunos códigos)
let subcadenaConSubstr: string = texto.substr(7, 6);
console.log(subcadenaConSubstr); // Salida: mundo
```

## Deep Dive
Históricamente, JavaScript ofrecía `substring`, `slice` y `substr` (este último considerado obsoleto) para extraer subcadenas. TypeScript, al ser un superset de JavaScript, respeta esas operaciones. La elección entre `substring` y `slice` a menudo se reduce a preferencias personales, aunque `slice` tiene la capacidad de aceptar índices negativos para empezar desde el final de la cadena. Esto puede ser especialmente útil en situaciones donde la longitud de la cadena no es fija. La implementación en el motor de JavaScript usa algoritmos eficientes que aseguran que la extracción sea rápida incluso en cadenas largas.

## See Also
- Referencia de TypeScript sobre Strings: [TypeScript Handbook - Strings](https://www.typescriptlang.org/docs/handbook/2/everyday-types.html#string)
- Documentación de Mozilla Developer Network sobre `String.prototype.substring()`: [MDN substring()](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/substring)
- Explicaciones sobre el método `slice`: [MDN slice()](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/slice)
