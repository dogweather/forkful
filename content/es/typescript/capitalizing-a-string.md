---
title:                "Capitalizando una cadena"
html_title:           "TypeScript: Capitalizando una cadena"
simple_title:         "Capitalizando una cadena"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/typescript/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Por qué

Capitalizar un string es una tarea común en la programación. Ya sea para mejorar la legibilidad del código o para cumplir con ciertas reglas de formato, es importante saber cómo capitalizar correctamente un string en TypeScript.

## Cómo hacerlo

Para capitalizar un string en TypeScript, podemos utilizar dos métodos diferentes: toUpperCase() y toLowerCase(). Ambos métodos convierten todos los caracteres de un string a mayúsculas o minúsculas, respectivamente.

```TypeScript
// Ejemplo de uso de toUpperCase()
let texto = "hola mundo";
console.log(texto.toUpperCase());
// Output: HOLA MUNDO

// Ejemplo de uso de toLowerCase()
let texto = "HOLA MUNDO";
console.log(texto.toLowerCase());
// Output: hola mundo
```

También podemos utilizar el método charAt() junto con toUpperCase() o toLowerCase() para capitalizar una sola letra específica dentro del string.

```TypeScript
let texto = "hola mundo";
console.log(texto.charAt(0).toUpperCase() + texto.slice(1));
// Output: Hola mundo
```

Si queremos capitalizar todas las palabras de un string, podemos utilizar el método replace() junto con expresiones regulares (\b = límite de una palabra).

```TypeScript
let texto = "hola mundo feliz";
console.log(texto.replace(/\b\w/g, l => l.toUpperCase()));
// Output: Hola Mundo Feliz
```

## Deep Dive

Es importante tener en cuenta que los métodos toUpperCase() y toLowerCase() no modifican el string original, sino que devuelven una nueva cadena de texto. De esta manera, siempre debemos asignar el resultado a una nueva variable o al mismo string original.

Además, es interesante mencionar que TypeScript también cuenta con el tipo de datos string literal, que nos permite especificar un valor concreto para un string. Por ejemplo:

```TypeScript
let color: "rojo" | "verde" | "azul";
color = "ROJO"; // Esto dará un error en tiempo de compilación
color = "rojo"; // Esto es correcto
```

Por último, también existen librerías como lodash o string.js que nos ofrecen métodos más avanzados para manipular strings, incluyendo la capitalización.

## Ver también

- [Documentación oficial de TypeScript](https://www.typescriptlang.org/)
- [Guía de referencia de String en TypeScript](https://www.typescriptlang.org/docs/handbook/basic-types.html#string)