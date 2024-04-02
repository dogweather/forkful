---
date: 2024-01-20 17:51:44.539615-07:00
description: "La interpolaci\xF3n de strings nos permite insertar valores de variables\
  \ dentro de cadenas de texto. Lo hacemos para construir mensajes din\xE1micos sin\u2026"
lastmod: '2024-03-13T22:44:58.784874-06:00'
model: gpt-4-1106-preview
summary: "La interpolaci\xF3n de strings nos permite insertar valores de variables\
  \ dentro de cadenas de texto. Lo hacemos para construir mensajes din\xE1micos sin\u2026"
title: "Interpolaci\xF3n de cadenas de texto"
weight: 8
---

## What & Why?
La interpolación de strings nos permite insertar valores de variables dentro de cadenas de texto. Lo hacemos para construir mensajes dinámicos sin ensuciar el código con un montón de concatenaciones.

## How to:
TypeScript, al igual que JavaScript, utiliza backticks (`` ` ``) para definir template literals, permitiéndonos insertar expresiones directamente usando `${expresión}`. Aquí te dejo unos ejemplos:

```TypeScript
let name: string = 'Mundo';
let message: string = `Hola, ${name}!`;
console.log(message); // Salida: Hola, Mundo!

let price: number = 9.99;
let product: string = 'libro';
console.log(`El precio del ${product} es ${price} euros.`); // Salida: El precio del libro es 9.99 euros.
```

## Deep Dive
Antes de ES6 (ECMAScript 2015), que es la versión en la que TypeScript se basa, la interpolación no existía en JavaScript. Teníamos que usar el operador `+` para concatenar variables y cadenas, lo que a veces resultaba engorroso:

```JavaScript
var name = 'Mundo';
var message = 'Hola, ' + name + '!';
// en TypeScript, también podríamos usar comillas simples o dobles de la misma manera
```

Ahora con las template strings, el código es más legible y mantenible. Además, TypeScript también verifica los tipos dentro de las interpolaciones, ayudándote a evitar errores tontos.

Una alternativa a la interpolación es usar la función `replace` con una expresión regular o con un marcador de posición como `%s`, aunque esto es menos intuitivo y más propenso a errores.

La interpolación de strings se realiza en tiempo de ejecución, compilando primero la plantilla de string y luego reemplazando las expresiones con los valores de las variables.

## See Also
- [Template Literals (MDN)](https://developer.mozilla.org/es/docs/Web/JavaScript/Reference/Template_literals) - Una guía detallada sobre template literals en JavaScript.
- [TypeScript Official Documentation](https://www.typescriptlang.org/docs/) - La documentación oficial de TypeScript, donde puedes aprender más sobre tipos, interfaces y otras funcionalidades.
