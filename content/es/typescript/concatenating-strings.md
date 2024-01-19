---
title:                "Concatenando cadenas de texto"
html_title:           "Arduino: Concatenando cadenas de texto"
simple_title:         "Concatenando cadenas de texto"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/typescript/concatenating-strings.md"
---

{{< edit_this_page >}}

## ¿Qué & Por Qué?

La concatenación de cadenas es la operación de unir dos o más cadenas de texto en una. Es útil en muchas tareas de programación, como la construcción de mensajes o la manipulación de datos.

## Cómo hacerlo:

Creamos dos variables `cadena1` y `cadena2` y luego las unimos mediante el operador '+'. Aquí tienes un ejemplo:

```TypeScript
let cadena1: string = "Hola";
let cadena2: string = " Mundo";

let resultado: string = cadena1 + cadena2; 

console.log(resultado); // "Hola Mundo"
```

El operador '+' une las cadenas `cadena1` y `cadena2` para producir una nueva cadena. Esencialmente, concatena `cadena1` con `cadena2`.

## Inmersión profunda

Historialmente, la concatenación de cadenas ha sido una operación fundamental en la computación desde sus primeros días. Las palabras se pueden tratar como una cadena de caracteres, y así la construcción de frases y párrafos se hace mediante la concatenación de estas palabras.

Existen alternativas a la concatenación de cadenas en TypeScript. Por ejemplo, se pueden usar plantillas de cadena, que permiten insertar valores de variables directamente en cadenas en tiempo de ejecución.

```TypeScript
let saludo: string = "Hola";
let objeto: string = "Mundo";

let mensaje: string = `${saludo} ${objeto}`;

console.log(mensaje); // "Hola Mundo"
```

Respecto a la implementación, la concatenación de cadenas en JavaScript (y por lo tanto, en TypeScript) es bastante rápida y eficiente en comparación con otros lenguajes como Java, donde concatenar cadenas puede ser costoso en tiempo de ejecución.

## Ver también:

Para más detalles sobre el tema y cómo usar de manera más eficiente la concatenación de cadenas en TypeScript, te recomendamos estos recursos:

- [TypeScript string concatenation](https://www.typescripttutorial.net/typescript-tutorial/typescript-string-concatenation/)
- [JavaScript String concatenation](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/concat)
- [TypeScript template strings](https://www.typescriptlang.org/docs/handbook/2/template-literal-types.html)