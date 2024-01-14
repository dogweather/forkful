---
title:                "Javascript: Buscando y reemplazando texto"
simple_title:         "Buscando y reemplazando texto"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/javascript/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Por qué

La búsqueda y reemplazo de texto es una habilidad imprescindible para cualquier programador, ya que permite hacer cambios rápidos y eficientes en el código. Además, es una herramienta útil para corregir errores y mejorar la legibilidad del código.

## Cómo hacerlo

Para realizar una búsqueda y reemplazo de texto en JavaScript, primero debemos utilizar el método `replace()` en una cadena de texto. Este método acepta dos argumentos: el texto que queremos reemplazar y el nuevo texto que queremos que lo sustituya. Por ejemplo:

```Javascript
let texto = "Hola mundo"
texto = texto.replace("Hola", "Adiós")
console.log(texto) // Salida: Adiós mundo
```

También podemos utilizar expresiones regulares para realizar un reemplazo más específico. En el siguiente ejemplo, reemplazamos todas las vocales por el carácter `x`:

```Javascript
let texto = "Hola mundo"
texto = texto.replace(/[aeiou]/g, "x")
console.log(texto) // Salida: Hxlx mxndx
```

## Profundizando

Existen diferentes opciones y funciones que podemos utilizar para hacer búsquedas y reemplazos más complejos en JavaScript. Por ejemplo, el método `search()` nos permite buscar un patrón en una cadena de texto y devuelve la posición de la primera coincidencia, mientras que `replaceAll()` nos permite reemplazar todas las ocurrencias de un texto en una cadena.

Otra herramienta útil es el constructor `RegExp`, que nos permite crear expresiones regulares con diferentes opciones y banderas para una búsqueda más precisa.

En resumen, conocer estas herramientas nos permite realizar búsquedas y reemplazos de manera más eficiente y precisa en nuestro código.

## Ver también

- [Documentación oficial de JavaScript sobre el método `replace()`](https://developer.mozilla.org/es/docs/Web/JavaScript/Reference/Global_Objects/String/replace)
- [Ejemplos prácticos de búsqueda y reemplazo en JavaScript](https://www.webdevdrops.com/javascript-replace/)
- [Cheat sheet de expresiones regulares en JavaScript](https://medium.com/factory-mind/regex-tutorial-a-simple-cheatsheet-by-examples-649dc1c3f285)