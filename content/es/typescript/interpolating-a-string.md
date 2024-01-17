---
title:                "Interpolando una cadena"
html_title:           "TypeScript: Interpolando una cadena"
simple_title:         "Interpolando una cadena"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/typescript/interpolating-a-string.md"
---

{{< edit_this_page >}}

## ¿Qué y por qué?
La interpolación de una cadena de texto es cuando los programadores insertan variables o expresiones en una cadena de texto para crear una nueva cadena personalizada. Esto permite a los programadores generar cadenas de texto dinámicas y reutilizables en su código.

## Cómo hacerlo:
```TypeScript
// Ejemplo de interpolación de cadena simple
let nombre = "Juan";
let edad = 30;
let mensaje = `Hola, mi nombre es ${nombre} y tengo ${edad} años.`

console.log(mensaje); // Salida: Hola, mi nombre es Juan y tengo 30 años.

// Ejemplo de interpolación de cadena con expresiones
let x = 5;
let y = 10;
let resultado = `La suma de ${x} y ${y} es: ${x + y}`;

console.log(resultado); // Salida: La suma de 5 y 10 es: 15
```

## Profundizando:
La interpolación de cadenas de texto se ha vuelto muy popular en los últimos años, con la introducción de ECMAScript2015 (ES6). Antes de ES6, los programadores tenían que concatenar manualmente cadenas de texto y variables, lo que resultaba en un código verboso y propenso a errores. Otra alternativa a la interpolación de cuerdas es el uso de la función `String.format()` en lenguajes como Java o C#, pero esto también puede ser engorroso y no es tan flexible como la interpolación de cadenas de texto.

La implementación de la interpolación de cadenas en TypeScript es similar a la de ES6, ya que TypeScript es un superset de JavaScript. Permite a los programadores crear cadenas de texto con variables o expresiones utilizando el símbolo de dolar y llaves (`${}`).

## Ver también:
- [Documentación oficial de TypeScript sobre interpolación de cadenas de texto](https://www.typescriptlang.org/docs/handbook/strings.html#string-interpolation)