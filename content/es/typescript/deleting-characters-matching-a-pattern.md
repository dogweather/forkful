---
title:                "Eliminando caracteres que coinciden con un patrón"
html_title:           "TypeScript: Eliminando caracteres que coinciden con un patrón"
simple_title:         "Eliminando caracteres que coinciden con un patrón"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/typescript/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## ¿Por qué alguien eliminaría caracteres que coincidan con un patrón?

A veces, cuando estamos trabajando con cadenas de texto en TypeScript, podemos encontrarnos en situaciones en las que necesitamos eliminar ciertos caracteres que siguen un patrón específico. Por ejemplo, puede que necesitemos eliminar todas las vocales de una palabra, o todos los dígitos de un número. Al saber cómo eliminar estos caracteres de manera eficiente, podemos mejorar el rendimiento de nuestro código y lograr resultados más precisos.

## Cómo hacerlo en TypeScript

Para eliminar caracteres que coincidan con un patrón en TypeScript, podemos utilizar el método `replace` de la clase `String`. Este método nos permite reemplazar partes de una cadena de texto por otra, siguiendo un cierto patrón. A continuación, mostraremos algunos ejemplos de cómo podemos utilizar este método:

```TypeScript
let str: string = "¡Hola mundo!";
let newStr = str.replace(/[aeiou]/g, ''); // Elimina todas las vocales
console.log(newStr); // Output: "! Hl nd!"

let num: string = "123-Hola-456";
let newNum = num.replace(/[0-9]/g, ''); // Elimina todos los dígitos
console.log(newNum); // Output: "Hola"
```

Como se puede ver en los ejemplos, utilizamos una expresión regular dentro del método `replace` para especificar el patrón que queremos eliminar. En el primer ejemplo, usamos `[aeiou]` para eliminar todas las vocales, mientras que en el segundo utilizamos `[0-9]` para eliminar los dígitos. Ambas expresiones se encuentran dentro de una expresión regular entre barras, y la opción `g` al final indica que se deben eliminar todos los caracteres que coincidan con el patrón, no solo la primera coincidencia.

## Profundizando en el tema

Aunque el método `replace` es una forma eficiente de eliminar caracteres que siguen un patrón, hay otras formas de lograrlo en TypeScript. Por ejemplo, podemos utilizar la función `filter` para filtrar los caracteres que queremos eliminar y luego unirlos nuevamente en una cadena. Además, también podemos utilizar la función `split` para dividir una cadena en un array de caracteres, eliminar los caracteres no deseados y luego unirlos nuevamente.

En general, eliminar caracteres que coincidan con un patrón puede parecer un proceso sencillo, pero puede ser realmente útil en situaciones en las que necesitamos limpiar y manipular cadenas de texto de manera eficiente.

## Ver también

- Documentación oficial de TypeScript sobre la clase `String`: https://www.typescriptlang.org/docs/handbook/classes.html#classes
- Artículo sobre expresiones regulares en TypeScript: https://dev.to/oyetoket/fast-regular-expressions-in-the-typescript-world-13m4
- Ejemplos de expresiones regulares en TypeScript: https://regex101.com/library/qR9mG0