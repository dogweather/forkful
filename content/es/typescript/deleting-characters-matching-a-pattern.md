---
title:                "TypeScript: Eliminando caracteres que coinciden con un patrón"
programming_language: "TypeScript"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/typescript/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## ¿Por qué eliminar caracteres que cumplen con un patrón?

A menudo, en la programación, nos encontramos con la necesidad de manipular y filtrar cadenas de texto para obtener información específica. Una tarea común es eliminar caracteres que cumplan con un patrón determinado. En esta entrada, te mostraremos cómo hacerlo utilizando TypeScript.

## Cómo hacerlo

Primero, debemos definir la cadena de texto en la que queremos buscar y eliminar los caracteres. Luego, utilizaremos la función `replace()` de TypeScript, pasando como parámetro una expresión regular (regex) que identifique el patrón que queremos eliminar. Por ejemplo, si queremos eliminar todas las vocales de una cadena de texto, podemos hacer lo siguiente:

```TypeScript
let cadena = "Hola mundo";
cadena = cadena.replace(/[aeiou]/g, "");
console.log(cadena); // Output: Hl mnd
```

En este ejemplo, la expresión regular `[aeiou]` busca cualquier vocal en la cadena y la función `replace()` la reemplaza por una cadena vacía. Al usar la bandera `g` al final de la expresión regular, nos aseguramos de que se eliminen todas las coincidencias y no solo la primera.

También podemos utilizar regex más complejas para eliminar un patrón específico. Por ejemplo, para eliminar los espacios en blanco que estén seguidos por una coma, podemos hacer lo siguiente:

```TypeScript
let cadena = "Hola, mundo";
cadena = cadena.replace(/\s+,/g, ",");
console.log(cadena); // Output: Hola,mundo
```

En este caso, utilizamos la expresión regular `\s+,` que busca cualquier espacio en blanco seguido por una coma y reemplaza ambas cosas por solo una coma.

## Profundizando

Para entender mejor cómo funciona la función `replace()` y las expresiones regulares, es importante tener en cuenta algunos conceptos. Primero, la función `replace()` reemplaza todas las coincidencias de la expresión regular en la cadena de texto original. Si solo queremos reemplazar la primera coincidencia, podemos usar la función `replace()` con un solo argumento, pasando una cadena de texto como parámetro en lugar de una expresión regular.

Además, las expresiones regulares pueden contener patrones y metacaracteres que nos permiten buscar patrones específicos en una cadena. Por ejemplo, el metacaracter `.` representa cualquier carácter, por lo que podemos usarlo para eliminar todo lo que está entre paréntesis, como en este ejemplo:

```TypeScript
let cadena = "Hola (mundo)";
cadena = cadena.replace(/\(.*\)/g, "");
console.log(cadena); // Output: Hola
```

En este caso, utilizamos la expresión regular `\(.*\)` que busca cualquier contenido entre paréntesis y lo reemplaza por una cadena vacía.

Como puedes ver, las expresiones regulares y la función `replace()` son herramientas poderosas para eliminar caracteres que cumplen con un patrón determinado en una cadena de texto.

## Ver también

- [Expresiones regulares en TypeScript](https://developer.mozilla.org/es/docs/Web/JavaScript/Guide/Regular_Expressions)
- [Documentación de TypeScript: función replace](https://www.typescriptlang.org/docs/handbook/strings.html#string-replace)