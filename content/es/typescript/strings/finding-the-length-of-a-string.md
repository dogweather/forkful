---
aliases:
- /es/typescript/finding-the-length-of-a-string/
date: 2024-01-20 17:48:15.930001-07:00
description: "En TypeScript, hallar la longitud de una cadena implica contar la cantidad\
  \ de caracteres que contiene. Saber esto es clave cuando manipulamos texto: para\u2026"
lastmod: 2024-02-18 23:09:09.693126
model: gpt-4-1106-preview
summary: "En TypeScript, hallar la longitud de una cadena implica contar la cantidad\
  \ de caracteres que contiene. Saber esto es clave cuando manipulamos texto: para\u2026"
title: Calculando la longitud de una cadena
---

{{< edit_this_page >}}

## What & Why?
En TypeScript, hallar la longitud de una cadena implica contar la cantidad de caracteres que contiene. Saber esto es clave cuando manipulamos texto: para validar entradas, subcadenas, o simplemente medir la información.

## How to:
La propiedad `length` devuelve el número de caracteres en una cadena:

```TypeScript
let saludo: string = "¡Hola, mundo!";
console.log(saludo.length); // 13
```

Output esperado: `13`

Si necesitas obtener la longitud de un string que viene de input del usuario, el proceso es idéntico:

```TypeScript
function imprimirLongitud(texto: string) {
  console.log(`La longitud del texto es: ${texto.length}`);
}

imprimirLongitud("TypeScript es genial"); // La longitud del texto es: 20
```

Output esperado: `La longitud del texto es: 20`

## Deep Dive
Históricamente, el concepto de contar caracteres viene desde los inicios de la informática. Fue, y sigue siendo, fundamental para la gestión de strings.

Alternativas para obtener la longitud de una cadena podrían ser iterar a través de sus caracteres o usar métodos de estructuras que modelan secuencias de caracteres. Sin embargo, en TypeScript, `length` es la forma nativa y eficiente para obtener esa información, ahorrando recursos al no crear overhead adicional de cómputo.

Una consideración a tener en cuenta es que `length` cuenta unidades de código UTF-16, por lo que podría dar conteos inesperados con caracteres no BMP (Basic Multilingual Plane). Por ejemplo:

```TypeScript
let emoji: string = "👍";
console.log(emoji.length); // 2
```

Output esperado: `2`

Este emoji es realmente un único carácter, pero como está fuera del plano multilingüe básico, se representa con dos unidades de código UTF-16, resultando en una longitud de 2.

## See Also
- MDN Web Docs sobre `length`: [String.length](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/length)
- Explicación de UTF-16 y caracteres no BMP: [Understanding UTF-16](https://unicodebook.readthedocs.io/unicode_encodings.html#utf-16-surrogate-pairs)
- Documentación oficial de TypeScript: [TypeScript Handbook](https://www.typescriptlang.org/docs/handbook/intro.html)
