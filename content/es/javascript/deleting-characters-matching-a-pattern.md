---
title:                "Javascript: Borrando caracteres que coinciden con un patrón"
programming_language: "Javascript"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/javascript/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Por qué

Eliminar caracteres que coinciden con un patrón es una tarea común en la programación. Ya sea para limpiar datos o para realizar una operación específica, conocer cómo hacer esto puede facilitarnos mucho el trabajo.

## Cómo hacerlo

Para eliminar caracteres que coinciden con un patrón en Javascript, podemos utilizar la función `replace()`. Esta función toma dos parámetros: el patrón a buscar y el reemplazo que deseamos realizar. Veamos un ejemplo:

```Javascript
// Definimos una cadena de texto
let texto = "Hola, soy un texto de ejemplo.";

// Utilizamos replace() para reemplazar la letra "a" por "o"
let textoModificado = texto.replace(/a/g, "o");

// Imprimimos el resultado
console.log(textoModificado); // "Holo, soy un texto de ejemplo."
```

En este ejemplo, utilizamos la expresión regular `/a/g`, que busca todas las apariciones de la letra "a" en la cadena de texto. Al utilizar la `g` al final de la expresión, le indicamos a Javascript que queremos buscar en toda la cadena y no solo en la primera aparición.

Podemos utilizar cualquier expresión regular como patrón, lo que nos permite realizar reemplazos más complejos. Además, también podemos utilizar una cadena de texto como reemplazo en lugar de una expresión regular.

## Profundizando

Para comprender mejor cómo funciona `replace()`, es importante entender que esta función no modifica la cadena de texto original, sino que devuelve una nueva cadena con las modificaciones realizadas. Esto significa que para guardar el resultado, debemos asignarlo a una nueva variable como en el ejemplo anterior.

También es importante tener en cuenta que `replace()` solo reemplaza la primera aparición del patrón por defecto. Para reemplazar todas las apariciones, debemos utilizar la `g` al final del patrón, como lo hicimos en el ejemplo.

Además, podemos utilizar `replace()` en combinación con otras funciones de Javascript, como `split()` o `substring()`, para realizar reemplazos más complejos.

## Ver también

- La documentación oficial de `replace()`: https://developer.mozilla.org/es/docs/Web/JavaScript/Reference/Global_Objects/String/replace
- Un tutorial sobre expresiones regulares en Javascript: https://www.w3schools.com/jsref/jsref_obj_regexp.asp
- Otros métodos útiles para manipular cadenas de texto en Javascript: https://www.freecodecamp.org/news/strings-and-their-methods-in-javascript-c4417241cf8e/