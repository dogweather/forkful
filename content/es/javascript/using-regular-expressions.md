---
title:                "Utilizando expresiones regulares"
html_title:           "Javascript: Utilizando expresiones regulares"
simple_title:         "Utilizando expresiones regulares"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/javascript/using-regular-expressions.md"
---

{{< edit_this_page >}}

## ¿Qué y por qué?

Las expresiones regulares en Javascript son patrones de búsqueda utilizados para verificar y manipular cadenas de texto. Los programadores las utilizan para encontrar y reemplazar partes específicas de cadenas de texto de manera más eficiente que con métodos tradicionales.

## ¿Cómo?

Para utilizar expresiones regulares en Javascript, simplemente necesitamos el objeto RegExp y el método ```.test()```. Por ejemplo:

```Javascript
let regex = new RegExp("patrón");
let string = "Esta es una cadena de texto con una palabra 'patrón' que queremos encontrar.";

console.log(regex.test(string));
```

Este código imprimirá ```true``` en la consola, ya que encontró el patrón en la cadena de texto. También podemos utilizar expresiones regulares directamente en los métodos de cadena de texto, como en el siguiente ejemplo:

```Javascript
let string = "Esta es otra cadena de texto con una palabra 'patrón' que queremos encontrar.";

console.log(string.match(/patrón/));
```

Este código también imprimirá ```true``` y nos mostrará más información sobre la coincidencia del patrón, como su posición en la cadena de texto.

## Profundizando

Las expresiones regulares se basan en una sintaxis desarrollada en la década de 1950 y han sido utilizadas en diferentes lenguajes de programación desde entonces. Además de la clase RegExp, también podemos utilizar el método ```.search()``` para buscar un patrón en una cadena de texto y el método ```.replace()``` para reemplazar una parte de la cadena de texto por otra.

## Ver también

- [Introducción a las expresiones regulares en MDN](https://developer.mozilla.org/es/docs/Web/JavaScript/Guide/Regular_Expressions)
- [Expresiones regulares en Javascript en W3Schools](https://www.w3schools.com/js/js_regexp.asp)