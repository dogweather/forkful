---
title:                "Buscando y reemplazando texto"
html_title:           "Javascript: Buscando y reemplazando texto"
simple_title:         "Buscando y reemplazando texto"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/javascript/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## ¿Por qué la búsqueda y reemplazo de texto es importante?

Una de las principales razones por las que la búsqueda y reemplazo de texto es una técnica valiosa en la programación es porque ayuda a ahorrar tiempo y esfuerzo al realizar cambios en grandes cantidades de texto. En lugar de tener que actualizar cada instancia manualmente, un solo comando puede hacer el trabajo de manera eficiente.

## Cómo hacer la búsqueda y reemplazo de texto en Javascript

La búsqueda y reemplazo de texto en Javascript es muy sencilla y se puede realizar utilizando el método `replace()`. Este método toma como argumento una expresión regular o una cadena de texto para encontrar y reemplazar. Veamos un ejemplo:

```Javascript
let texto = "Hola, mi nombre es Juan";
// Reemplazando "Juan" por "Pedro"
let nuevoTexto = texto.replace("Juan", "Pedro");
console.log(nuevoTexto);
// Output: "Hola, mi nombre es Pedro"
```

Además de cadenas de texto, también se pueden utilizar expresiones regulares para realizar búsquedas más avanzadas. Por ejemplo, si queremos reemplazar todas las vocales en una cadena de texto por la letra "x", podemos hacerlo de la siguiente manera:

```Javascript
let texto = "Hola, mi nombre es Maria";
// Reemplazando todas las vocales por "x"
let nuevoTexto = texto.replace(/[aeiou]/gi, "x");
console.log(nuevoTexto);
// Output: "Hxlx, mx nxmbrx xs Mxrx"
```

## Un poco más profundo en la búsqueda y reemplazo de texto

Ahora que hemos visto cómo realizar la búsqueda y reemplazo básico en Javascript, vale la pena señalar que también existen opciones adicionales en el método `replace()`. Por ejemplo, se pueden utilizar `g` y `i` al final de la expresión regular para indicar que la búsqueda debe ser global (reemplazar todas las instancias) y no sensible a mayúsculas y minúsculas, respectivamente. También se pueden utilizar funciones para determinar el texto de reemplazo en lugar de solo una cadena de texto.

## Ver también
- Documentación de replace() en MDN: https://developer.mozilla.org/es/docs/Web/JavaScript/Reference/Global_Objects/String/replace