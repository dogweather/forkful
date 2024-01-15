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

## Por qué 

Extraer subcadenas o subconjuntos de una cadena de texto es una tarea común en el mundo de la programación. Esto puede ser útil para realizar tareas como validar una entrada de usuario, formatear datos de manera específica o simplemente manipular cadenas de texto para su uso en otras funciones. Al aprender cómo extraer subcadenas, tendrás una herramienta más en tu cinturón de herramientas de programación.

## Cómo hacerlo

Extraer subcadenas de una cadena de texto puede ser fácil y sencillo con el uso de algunos métodos y funciones incorporados en Javascript. A continuación, se muestra un ejemplo de cómo extraer una subcadena de una cadena de texto usando el método `substring()`:

``` Javascript
// Definimos una cadena de texto
let texto = "¡Hola a todos!";
// Extraemos la subcadena desde el índice 5 hasta el final
let subcadena = texto.substring(5);
// Imprimimos la subcadena en la consola
console.log(subcadena); // salida: "a todos!"
```

En este ejemplo, usamos el método `substring()` para extraer una subcadena desde el índice 5 hasta el final de la cadena de texto. También se puede proporcionar un segundo parámetro que especifique el índice final de la subcadena. Además de `substring()`, también puedes usar otros métodos como `slice()`, `substr()` y `split()` para lograr resultados similares. 

## Profundizando 

Para comprender mejor cómo funcionan estos métodos y funciones de extracción de subcadenas, es importante tener en cuenta el concepto de índices en una cadena de texto. Cada carácter en una cadena de texto tiene un índice correspondiente, comenzando desde 0 hasta la longitud de la cadena menos 1. Esto significa que el primer carácter tiene un índice de 0, el segundo tiene un índice de 1 y así sucesivamente. 

Al pasar un índice como argumento a un método de extracción de subcadenas, se indica dónde comenzar o terminar la extracción. Por ejemplo, en el primer ejemplo, pasamos un índice de 5 para indicar que la subcadena debe comenzar desde el sexto carácter (índice 5) hasta el final de la cadena. 

Otro detalle importante a tener en cuenta es que estos métodos de extracción de subcadenas no modifican la cadena original, sino que devuelven una nueva cadena con la subcadena extraída. Por lo tanto, es importante almacenar el resultado en una nueva variable o imprimirlo en la consola para ver el resultado.

## Ver también

- Documentación de MDN sobre la función `substring()`: https://developer.mozilla.org/es/docs/Web/JavaScript/Reference/Global_Objects/String/substring
- Artículo sobre cómo manipular cadenas de texto en Javascript: https://www.freecodecamp.org/news/javascript-string-manipulation-how-to-manipulate-strings-in-javascript/
- Video tutorial sobre extracción de subcadenas en Javascript: https://www.youtube.com/watch?v=QZ5qOVS6WVc