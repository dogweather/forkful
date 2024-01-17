---
title:                "Utilizando expresiones regulares"
html_title:           "TypeScript: Utilizando expresiones regulares"
simple_title:         "Utilizando expresiones regulares"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/typescript/using-regular-expressions.md"
---

{{< edit_this_page >}}

## ¿Qué y por qué?
Las expresiones regulares son patrones utilizados para encontrar texto dentro de una cadena de caracteres. Los programadores las utilizan para buscar y manipular información de manera más eficiente en programas y aplicaciones. 

## Cómo hacerlo:
Las expresiones regulares en TypeScript se pueden crear utilizando el constructor `RegExp` o utilizando la notación `/expresión/`. Por ejemplo, para encontrar todas las letras mayúsculas en una cadena de texto, se puede utilizar el siguiente código:

```TypeScript
const texto = "Hola MUNDO";
const expresion = /[A-Z]/g;
const letrasMayusculas = texto.match(expresion);
console.log(letrasMayusculas); // Output: ["H", "M"]
```
El `g` al final de la expresión indica que se buscan todas las ocurrencias de las letras mayúsculas.

## Profundizando:
Las expresiones regulares tienen sus raíces en la teoría matemática y lógica, y han sido utilizadas en informática desde la década de 1950. Aunque pueden ser poderosas, también pueden ser complicadas de entender y escribir. Algunas alternativas a las expresiones regulares incluyen el uso de métodos de cadenas incorporados en TypeScript, como `includes()` o `replace()`. También hay bibliotecas de terceros disponibles que ofrecen funcionalidades y sintaxis más amigables para trabajar con patrones en cadenas de texto. 

## Ver también:
- [Documentación de TypeScript sobre expresiones regulares](https://www.typescriptlang.org/docs/handbook/regular-expressions.html)
- [Expresiones regulares en JavaScript (sintaxis similar a TypeScript)](https://developer.mozilla.org/es/docs/Web/JavaScript/Guide/Regular_Expressions)
- [Biblioteca `XRegExp` para expresiones regulares mejoradas en JavaScript](https://xregexp.com/)