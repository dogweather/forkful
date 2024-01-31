---
title:                "Uso de expresiones regulares"
date:                  2024-01-19
simple_title:         "Uso de expresiones regulares"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/typescript/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Qué y Por Qué?
Las expresiones regulares (regex) detectan patrones en texto. Los programadores las usan para buscar, validar, manipular y analizar datos de manera potente y flexible.

## Cómo hacerlo

```TypeScript
// Definimos una regex para validar un email
const regexEmail: RegExp = /^[^\s@]+@[^\s@]+\.[^\s@]+$/;

// Función para validar email con nuestra regex
function validarEmail(email: string): boolean {
  return regexEmail.test(email);
}

// Uso de la función
console.log(validarEmail('usuario@ejemplo.com')); // true
console.log(validarEmail('usuario_mal_formado.com')); // false
```

## Inmersión Profunda
Las regex surgieron en la década de 1950 con el matemático Stephen Kleene. Ahora están en todos los lenguajes de programación modernos. Otras herramientas como bibliotecas de análisis sintáctico (parsers) ofrecen alternativas para ciertos casos de uso. La implementación de regex en TypeScript se realiza a través del objeto `RegExp` de JavaScript, dado que TypeScript es un superset de JavaScript.

## Ver También
- [MDN Web Docs Regular Expressions](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Guide/Regular_Expressions)
- [Regex Tester](https://regexr.com/)
- [TypeScript Documentation](https://www.typescriptlang.org/docs/)
