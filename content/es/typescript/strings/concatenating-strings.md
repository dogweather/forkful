---
date: 2024-01-20 17:35:37.738477-07:00
description: "Concatenar cadenas es simplemente juntar dos o m\xE1s trozos de texto\
  \ en uno solo. Lo hacemos porque a veces necesitamos construir mensajes personalizados\
  \ o\u2026"
lastmod: '2024-03-13T22:44:58.791566-06:00'
model: gpt-4-1106-preview
summary: "Concatenar cadenas es simplemente juntar dos o m\xE1s trozos de texto en\
  \ uno solo. Lo hacemos porque a veces necesitamos construir mensajes personalizados\
  \ o\u2026"
title: "Concatenaci\xF3n de cadenas de texto"
---

{{< edit_this_page >}}

## ¿Qué es y por qué?
Concatenar cadenas es simplemente juntar dos o más trozos de texto en uno solo. Lo hacemos porque a veces necesitamos construir mensajes personalizados o combinar información de diferentes fuentes.

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
