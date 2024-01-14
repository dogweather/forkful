---
title:                "TypeScript: Buscando y reemplazando texto"
programming_language: "TypeScript"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/typescript/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Por qué

En programación, a menudo nos encontramos con la necesidad de realizar modificaciones en nuestro código. Una de estas tareas comunes es la de buscar y reemplazar texto. Ya sea para corregir errores, actualizar nombres de variables o realizar cambios en todo el proyecto, es importante saber cómo hacerlo de manera eficiente y efectiva.

## Cómo hacerlo

En TypeScript, podemos utilizar el método `replace()` para realizar la búsqueda y reemplazo de texto. Este método acepta dos parámetros: el texto a buscar y el texto a reemplazar.

```TypeScript
let text = "Hola mundo";
let newText = text.replace("mundo", "tierra");
console.log(newText); // Salida: Hola tierra
```

En este ejemplo, hemos reemplazado la palabra "mundo" por "tierra" en la variable `text` y hemos almacenado el resultado en la variable `newText`. Podemos ver que se ha realizado el reemplazo correctamente al imprimir el valor de `newText` en la consola.

Si deseamos realizar un reemplazo en todo el texto, podemos agregar la letra `g` al final del primer parámetro. Esto indica que queremos hacer el reemplazo globalmente.

```TypeScript
let sentence = "¡Hola a todos!";
let newSentence = sentence.replace(/a/g, "e");
console.log(newSentence); // Salida: ¡Hole e todos!
```

En este ejemplo, hemos utilizado una expresión regular entre barras diagonales para especificar que queremos reemplazar todas las letras "a" por "e" en la variable `sentence`.

## Profundizando

El método `replace()` también nos permite utilizar expresiones regulares más avanzadas para realizar la búsqueda y reemplazo de texto. Por ejemplo, podemos utilizar el operador de alternancia `|` para especificar varias opciones de reemplazo.

```TypeScript
let text = "Hola a todos";
let newText = text.replace(/a|o/g, "i");
console.log(newText); // Salida: Hili i titis
```

En este ejemplo, hemos utilizado una expresión regular para reemplazar todas las letras "a" y "o" por "i" en la variable `text`.

También podemos utilizar capturas de grupos en nuestras expresiones regulares para almacenar el texto encontrado y utilizarlo en el reemplazo.

```TypeScript
let sentence = "El número pi es 3.1416";
let newSentence = sentence.replace(/(\d+(\.\d+)?)/g, "$16");
console.log(newSentence); // Salida: El número pi es 16
```

En este ejemplo, hemos utilizado una expresión regular para buscar y almacenar el número decimal en la variable `sentence` y luego lo hemos utilizado en el reemplazo junto con el texto "16".

## Ver también

- Documentación oficial de TypeScript sobre el método `replace()`: https://www.typescriptlang.org/docs/handbook/release-notes/typescript-4-1.html#the-infamous-regex-replace-method
- Ejemplos de expresiones regulares en TypeScript: https://dev.to/bparashkevov/regex-lecciones-basicas-javascript-4p9n