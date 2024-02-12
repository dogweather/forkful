---
title:                "Concatenación de cadenas de texto"
aliases:
- /es/typescript/concatenating-strings/
date:                  2024-01-20T17:35:37.738477-07:00
model:                 gpt-4-1106-preview
simple_title:         "Concatenación de cadenas de texto"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/typescript/concatenating-strings.md"
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
