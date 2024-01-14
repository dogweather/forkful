---
title:                "Javascript: Extrayendo subcadenas"
programming_language: "Javascript"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/javascript/extracting-substrings.md"
---

{{< edit_this_page >}}

## Por qué
Extraer subcadenas de una cadena de caracteres es una herramienta común y útil en la programación. Puede ayudarnos a manipular y analizar cadenas de texto de manera más efectiva, lo que puede ser especialmente útil en proyectos web que involucren formularios, validación de datos y búsqueda de patrones.

## Cómo hacerlo
La forma más sencilla de extraer subcadenas en Javascript es utilizando el método `substring()`. Este método toma dos índices como argumentos y devuelve la subcadena ubicada entre esos índices. Por ejemplo:

```Javascript
let cadena = "Esta es una cadena de texto";
cadena.substring(0, 4); // devuelve "Esta"
cadena.substring(5, 7); // devuelve "es"
cadena.substring(11); // devuelve "cadena de texto"
```
Es importante tener en cuenta que los índices comienzan en cero y que el segundo argumento es opcional. Si no se especifica, el método devolverá la subcadena desde el índice especificado hasta el final de la cadena.

Otra forma de extraer subcadenas es utilizando el método `slice()`, que funciona de manera similar a `substring()`, pero permite utilizar índices negativos para indicar la posición desde el final de la cadena.

```Javascript
let cadena = "Esta es una cadena de texto";
cadena.slice(-5); // devuelve "texto"
cadena.slice(11, -6); // devuelve "cadena"
cadena.slice(5, -14); // devuelve "es"
```

## Profundizando
Existen otras formas de extraer subcadenas en Javascript, como utilizar expresiones regulares, el método `split()` o el operador de acceso a cadenas `[]`. También es importante tener en cuenta que los índices pueden ser utilizados con otros métodos de manipulación de cadenas, como `replace()` o `indexOf()`.

También es posible extraer múltiples subcadenas utilizando un bucle y almacenarlas en un array para posteriormente utilizarlas en nuestro código. Esto puede ser especialmente útil en proyectos que involucren la traducción de cadenas a diferentes idiomas, donde se deben extraer partes específicas de la cadena original.

## Ver también
- [Documentación oficial de Mozilla sobre el método `substring()` en Javascript](https://developer.mozilla.org/es/docs/Web/JavaScript/Reference/Global_Objects/String/substring)
- [Método `substring()` vs `slice()` en Javascript (en inglés)](https://love2dev.com/blog/javascript-substring-slice/)
- [Manipulación de cadenas en Javascript (en inglés)](https://www.freecodecamp.org/news/javascript-string-manipulation/#:~:text=JavaScript%20string%20manipulation%20refers%20to,characters%20in%20a%20string%20literal.)
- [Ejemplos de uso de subcadenas en proyectos web (en inglés)](https://www.w3schools.com/TAGS/tryit.asp?filename=tryhtml_input_testpattern)

¡Feliz programación! 🚀