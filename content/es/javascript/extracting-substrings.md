---
title:                "Extrayendo subcadenas"
html_title:           "Javascript: Extrayendo subcadenas"
simple_title:         "Extrayendo subcadenas"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/javascript/extracting-substrings.md"
---

{{< edit_this_page >}}

## ¿Qué y por qué?
Extraer subcadenas es el proceso de obtener una parte específica de una cadena más larga. Los programadores hacen esto para manipular y trabajar con cadenas de texto de manera más precisa y eficiente.

## Cómo:
```javascript
// Crear una cadena de texto
let nombre = "Juanito Perez";

// Extraer la primera letra
let primeraLetra = nombre.substring(0, 1);
console.log(primeraLetra); // Output: J

// Extraer una subcadena desde el índice 6 hasta el final
let apellido = nombre.substring(6);
console.log(apellido); // Output: Perez
```

## Profundizando:
Extraer subcadenas es una técnica comúnmente usada en programación, que ha sido posible gracias a la incorporación de métodos y funciones específicas para manejar cadenas en muchos lenguajes de programación. Alternativas a este método incluyen el uso de expresiones regulares para encontrar y extraer patrones específicos en una cadena de texto. En JavaScript, el método `substring()` permite extraer subcadenas especificando el índice de inicio y fin deseado.

## Ver también:
- [MDN Web Docs: String.substring()](https://developer.mozilla.org/es/docs/Web/JavaScript/Reference/Global_Objects/String/substring)
- [W3Schools: JavaScript Substrings](https://www.w3schools.com/js/js_string_sub.asp)
- [FreeCodeCamp: JavaScript Substrings](https://www.freecodecamp.org/news/how-to-use-the-javascript-substring-method/)