---
date: 2024-01-20 17:35:37.738477-07:00
description: "As\xED se hace: Concatenar cadenas es fundamental. Hist\xF3ricamente,\
  \ se usaba el operador `+`, y en lenguajes como Java, esto podr\xEDa ser costoso\
  \ en t\xE9rminos de\u2026"
lastmod: '2024-04-05T21:54:00.142010-06:00'
model: gpt-4-1106-preview
summary: Concatenar cadenas es fundamental.
title: "Concatenaci\xF3n de cadenas de texto"
weight: 3
---

## Así se hace:
```TypeScript
let saludo = "Hola, ";
let nombre = "Mundo!";
let mensajeCompleto = saludo + nombre; // Concatenación con el operador +
console.log(mensajeCompleto); // "Hola, Mundo!"

// Con plantillas de literales (template literals)
let mensajeTemplateLiteral = `${saludo}${nombre}`;
console.log(mensajeTemplateLiteral); // "Hola, Mundo!"

```

## Profundizando
Concatenar cadenas es fundamental. Históricamente, se usaba el operador `+`, y en lenguajes como Java, esto podría ser costoso en términos de rendimiento debido a la inmutabilidad de las cadenas. TypeScript, por otro lado, al ser un superset de JavaScript, comparte sus características, incluyendo plantillas literales introducidas con ES6 que ofrecen una sintaxis más limpia y capacidades como interpolación. Las plantillas de literales también permiten incorporar expresiones, lo que puede simplificar muchos casos de uso complicados.

## Véase también:
- [Manual de TypeScript](https://www.typescriptlang.org/docs/)
- [Template literals en TypeScript/JavaScript](https://developer.mozilla.org/es/docs/Web/JavaScript/Reference/Template_literals)
- [String concatenation in JavaScript](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Operators/Addition#string_concatenation)
