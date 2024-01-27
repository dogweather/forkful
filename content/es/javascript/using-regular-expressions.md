---
title:                "Uso de expresiones regulares"
date:                  2024-01-19
html_title:           "Arduino: Uso de expresiones regulares"
simple_title:         "Uso de expresiones regulares"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/javascript/using-regular-expressions.md"
---

{{< edit_this_page >}}

## ¿Qué y Por Qué?

Usar expresiones regulares es buscar patrones en strings. Programadores las utilizan para validar, remplazar y extraer info con precisión.

## How to:

```javascript
// Validar formato de email
const validarEmail = (email) => /^[^\s@]+@[^\s@]+\.[^\s@]+$/.test(email);

console.log(validarEmail('ejemplo@correo.com')); // true
console.log(validarEmail('esto-no-es-un-email')); // false

// Encontrar todas las coincidencias de un patrón
const texto = 'Los números son 123, 456 y 7890.';
const regex = /\b\d+\b/g; // \d es un dígito. + significa uno o más. \b límite de palabra.
const coincidencias = texto.match(regex);

console.log(coincidencias); // ['123', '456', '7890']

// Reemplazar palabras en una frase
const frase = 'Javascript es asombroso, Javascript es divertido.';
const fraseModificada = frase.replace(/Javascript/g, 'Python');

console.log(fraseModificada); // 'Python es asombroso, Python es divertido.'
```

## Deep Dive

Las expresiones regulares nacieron en los años 1950 y fueron formalizadas por matemático Stephen Kleene. Alternativas modernas incluyen parsers y bibliotecas especializadas, pero las "regex" siguen siendo únicas por su potencia y versatilidad en la manipulación de strings. En Javascript, regex se implementan como objetos `RegExp` o literales regex entre barras.

## See Also

- [MDN Web Docs: Expresiones Regulares](https://developer.mozilla.org/es/docs/Web/JavaScript/Guide/Regular_Expressions)
- [RegExr: Herramienta para aprender y probar expresiones regulares](https://regexr.com/)
- [Wikipedia: Expresión Regular](https://es.wikipedia.org/wiki/Expresi%C3%B3n_regular)
