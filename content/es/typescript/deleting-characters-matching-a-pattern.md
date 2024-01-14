---
title:                "TypeScript: Eliminando caracteres que coinciden con un patrón"
simple_title:         "Eliminando caracteres que coinciden con un patrón"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/typescript/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Por qué

A veces en la programación, es necesario eliminar ciertos caracteres de una cadena de texto que coincidan con un patrón específico. Esto puede ser útil para limpiar datos o para filtrar información importante. En TypeScript, hay formas sencillas de lograr esto y en este artículo, ¡te mostraremos cómo hacerlo!

## Cómo hacerlo

Imaginemos que tenemos una cadena de texto que contiene una serie de números y queremos eliminar todos los dígitos de ella. Podemos lograr esto utilizando el método `replace`, que toma dos parámetros: el patrón que deseamos reemplazar y el nuevo valor que queremos poner en su lugar. Por ejemplo:

```TypeScript
let cadenaDeTexto = "Esta es una cadena de texto: 123456789";
let nuevaCadena = cadenaDeTexto.replace(/[0-9]/g, '');
console.log(nuevaCadena);
```

Este código imprimirá `Esta es una cadena de texto:` eliminando todos los dígitos de la cadena original.

## Profundizando

Como se mencionó antes, el método `replace` toma un patrón y un nuevo valor como parámetros. El patrón en este caso es una expresión regular, que también se conoce como regex. Una expresión regular es una secuencia de caracteres que define un patrón de búsqueda. En el ejemplo anterior, usamos `[0-9]` como patrón, lo que significa que cualquier dígito del 0 al 9 será reemplazado. También podemos utilizar otros caracteres para lograr diferentes resultados, por ejemplo:

- `.`: coincide con cualquier carácter
- `\s`: coincide con cualquier espacio en blanco
- `[a-z]`: coincide con cualquier letra minúscula

Además, el modificador `g` después del patrón significa que la búsqueda se aplicará a toda la cadena en lugar de solo a la primera coincidencia.

Existen muchas otras formas de utilizar expresiones regulares en TypeScript para eliminar caracteres que coincidan con un patrón específico. ¡Te animamos a que investigues más y pruebes diferentes combinaciones para lograr tu objetivo!

## Ver también

¡Aquí hay unos enlaces útiles para que descubras más formas de utilizar expresiones regulares en TypeScript!

- [Introducción a las expresiones regulares en TypeScript](https://www.typescriptlang.org/docs/handbook/regular-expressions.html)
- [Especificación de expresiones regulares en TypeScript](https://developer.mozilla.org/es/docs/Web/JavaScript/Guide/Regular_Expressions)
- [Pruebas de expresiones regulares en línea](https://regex101.com/) (en inglés)