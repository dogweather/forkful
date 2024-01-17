---
title:                "Interpolando una cadena"
html_title:           "Javascript: Interpolando una cadena"
simple_title:         "Interpolando una cadena"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/javascript/interpolating-a-string.md"
---

{{< edit_this_page >}}

## ¿Qué y por qué?

Interpolar una string en Javascript significa insertar variables o expresiones dentro de una cadena de texto para crear una nueva cadena personalizada. Los programadores lo utilizan para hacer que el código sea más dinámico y poder utilizar el mismo formato para diferentes valores.

## Cómo:

Para interpolar una string, podemos utilizar la sintaxis de "template literals" o plantillas literales en Javascript. Esto se hace colocando las variables o expresiones entre ${}. Por ejemplo:

```Javascript
let nombre = "Juan";
console.log(`¡Hola ${nombre}! ¿Cómo estás?`);
```

La salida de este código sería: ¡Hola Juan! ¿Cómo estás?

## Profundizando:

La interpolación de strings se introdujo en ECMAScript 2015 (también conocido como ES6) como parte de las mejoras en la sintaxis de Javascript. Antes, los programadores tenían que concatenar cadenas y variables utilizando el operador +, lo cual puede resultar confuso y propenso a errores.

Otra alternativa a la interpolación de strings en Javascript es utilizar la función `sprintf` de la librería `sprintf-js`. Sin embargo, esto implica añadir una dependencia externa a nuestro código.

En cuanto a la implementación, el motor de Javascript convierte automáticamente las plantillas literales en una cadena de texto y los valores interpolados son tratados como expresiones regulares. Esto significa que podemos utilizar operadores, funciones y cualquier otra expresión válida dentro de la interpolación.

## Ver también:

- [String interpolation in Javascript](https://www.w3schools.com/js/js_string_interpolation.asp)
- [Template literals in Javascript](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Template_literals)
- [sprintf-js library](https://www.npmjs.com/package/sprintf-js)