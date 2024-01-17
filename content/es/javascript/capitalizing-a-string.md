---
title:                "Cambiar una cadena a mayúsculas"
html_title:           "Javascript: Cambiar una cadena a mayúsculas"
simple_title:         "Cambiar una cadena a mayúsculas"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/javascript/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## ¿Qué y por qué?
Capitalizar una cadena de texto simplemente significa convertirla en una cadena en la que la primera letra de cada palabra esté en mayúscula. Los programadores suelen hacer esto para mejorar la legibilidad y la presentación de sus códigos.

## Cómo hacerlo:
Hay varias formas de capitalizar una cadena en JavaScript. Puedes usar la función ```toUpperCase()``` para convertir la cadena completa en mayúsculas, o la función ```charAt()``` junto con ```toUpperCase()``` para convertir solo la primera letra en mayúscula. Ejemplos de código y salidas de muestra a continuación:

```
// Convertir la cadena completa en mayúsculas
let cadena = "hola mundo";
let cadenaCapitalizada = cadena.toUpperCase();
console.log(cadenaCapitalizada); // Salida: HOLA MUNDO

// Convertir solo la primera letra en mayúscula
let cadena = "hola mundo";
let primeraLetra = cadena.charAt(0).toUpperCase();
let cadenaCapitalizada = primeraLetra + cadena.slice(1);
console.log(cadenaCapitalizada); // Salida: Hola mundo
```

## Profundizando:
La capitalización de cadenas ha existido desde los primeros días de la informática y se utiliza en muchos lenguajes de programación. Alternativas en JavaScript incluyen el uso de expresiones regulares y la función ```replace()```. La implementación de la función ```toUpperCase()``` varía dependiendo de la codificación del lenguaje utilizado.

## Ver también:
- [Documentación de JavaScript para la función toUpperCase()](https://developer.mozilla.org/es/docs/Web/JavaScript/Referencia/Objetos_globales/String/toUpperCase)
- [Ejemplos de expresiones regulares para capitalizar cadenas en JavaScript](https://www.regexpal.com/39242)