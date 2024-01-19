---
title:                "Interpolando una cadena de texto"
html_title:           "Haskell: Interpolando una cadena de texto"
simple_title:         "Interpolando una cadena de texto"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/typescript/interpolating-a-string.md"
---

{{< edit_this_page >}}

## ¿Qué y Por qué?

La interpolación de strings en TypeScript es la inserción de variables u expresiones dentro de un string. Esto puede facilitar la codificación ya que permite insertar variables directamente en un string en lugar de concatenarlas.

## Cómo hacerlo:

La interpolación de cadenas en TypeScript se hace utilizando el carácter backtick (` `) y poniendo la variable o expresión dentro de ${}. Aquí se muestra un ejemplo:

```TypeScript
let nombre = "Juan";
let saludo = `Hola, ${nombre}`;
console.log(saludo);  // Output: Hola, Juan
```

Si combinas con una expresión, la expresión será evaluada y el resultado ser insertada en la string:

```TypeScript
let a = 10;
let b = 5;
let suma = `La suma de ${a} y ${b} es ${a+b}`;
console.log(suma);  // Output: La suma de 10 y 5 es 15
```

## Inmersión Profunda

La interpolación de cadenas, también conocida como las "plantillas de cadena" en TypeScript, fue introducida en ES6 y luego adoptada por TypeScript. Antes de ES6, debías concatenar las variables con el string, lo que podía ser un proceso largo y confuso especialmente para strings largas y con muchas variables.

Existen alternativas a la interpolación de cadenas, como la concatenación de strings, pero estas alternativas pueden requerir más código y pueden resultar en código más difícil de leer.

En cuanto a los detalles de implementación, las plantillas de cadenas en TypeScript se implementan como instancias del objeto global String. Eso significa que tienes acceso a todos los métodos que un objeto string normal tendría, incluyendo .length, .indexOf(), etc.

## Ver También

Para obtener más información sobre la interpolación de cadenas en TypeScript y otras características de TypeScript, puedes revisar los siguientes recursos:

- "Interpolación de Cadenas y Plantillas"
(https://typescript.ninja/typescript/es6-template-strings)
- Documentación Oficial de TypeScript (https://www.typescriptlang.org/docs/)
- Guía de TypeScript en MDN 
(https://developer.mozilla.org/en-US/docs/Web/JavaScript/Guide/Text_formatting_with _template_literals)