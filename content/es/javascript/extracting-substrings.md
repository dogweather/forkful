---
title:                "Extrayendo subcadenas"
html_title:           "C: Extrayendo subcadenas"
simple_title:         "Extrayendo subcadenas"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/javascript/extracting-substrings.md"
---

{{< edit_this_page >}}

## ¿Qué y Por qué?

Extraer substrings consiste en conseguir partes específicas de una cadena de texto. Es útil para analizar y manipular datos, permitiéndonos enfocarnos en información relevante dentro de una cadena mayor.

## ¿Cómo hacerlo?

Para extraer substrings en JavaScript, tenemos el método `substring()` y `slice()`.

```Javascript
var texto = "Hola Mundo";
var sub_string = texto.substring(0, 4);
console.log(sub_string);  // Produce: "Hola"
```

El código anterior muestra cómo utilizar la función `substring()`. Los parámetros (0, 4) indican que queremos extraer los caracteres desde la posición 0 hasta la 4 (sin incluir el 4).

```Javascript
var texto = "Hola Mundo";
var sub_string = texto.slice(0, 4);
console.log(sub_string);  // Produce: "Hola"
```

La función `slice()` trabaja de manera similar a `substring()`.

## Un Viaje Más Profundo

Métodos para extraer substrings existen desde los primeros días de la programación, permitiendo a los desarrolladores interactuar con cadenas de texto de una manera más detallada y precisa.

Aunque ambos, `substring()` y `slice()`, pueden ser usados para extraer substrings, difieren en cómo manejan los índices negativos. `slice()`, a diferencia de `substring()`, acepta índices negativos. Un índice negativo indica una posición empezando desde el final de la cadena.

```Javascript
var texto = "Hola Mundo";
var sub_string = texto.slice(-5);
console.log(sub_string);  // Produce: "Mundo"
```

Incluso, hay una alternativa más moderna, `substr()`, que también permite índices negativos, pero su uso no es recomendable debido a que esta obsoleto y no es estándar.

## Ver También

Para más detalles y ejemplos de substrings, puedes consultar las siguientes fuentes:

- [MDN: substring()](https://developer.mozilla.org/es/docs/Web/JavaScript/Reference/Global_Objects/String/substring)
- [MDN: slice()](https://developer.mozilla.org/es/docs/Web/JavaScript/Reference/Global_Objects/String/slice)