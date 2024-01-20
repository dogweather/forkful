---
title:                "Capitalizando una cadena de texto"
html_title:           "Arduino: Capitalizando una cadena de texto"
simple_title:         "Capitalizando una cadena de texto"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/javascript/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Qué y por qué?
Capitalizar un texto en JavaScript significa convertir la primera letra de una cadena a mayúscula. Es útil para nombres, títulos o siempre que necesitas que tu texto se vea formal y pulido.

## Cómo hacerlo:
Para capitalizar una cadena, crearemos una función llamada `capitalizar` y usaremos métodos de cadenas como `slice()` y `toUpperCase()`.

```javascript
function capitalizar(cadena) {
  if(cadena && typeof cadena === 'string') {
    return cadena.charAt(0).toUpperCase() + cadena.slice(1);
  }
  return cadena;
}

console.log(capitalizar("hola mundo")); // "Hola mundo"
```

Este código verifica primero si `cadena` existe y es una cadena de texto. Después, capitaliza la primera letra y la une con el resto de la cadena.

## Deep Dive
Históricamente, JavaScript no incluyó una función de capitalización como tal. Esto llevó a los desarrolladores a crear sus propias soluciones, comúnmente a través de funciones como la que acabamos de hacer.

Alternativas para capitalizar incluyen el uso de expresiones regulares o librerías de terceros como Lodash con su método `_.capitalize`. Cada opción tiene sus propio pros y contras en términos de rendimiento y legibilidad.

En términos de implementación:
- `charAt(0).toUpperCase()` obtiene y capitaliza la primera letra.
- `slice(1)` obtiene el resto de la cadena.
- Asegúrate de manejar casos en los que el input no sea una cadena o esté vacío.

## Ver también
Para ampliar tus conocimientos y explorar más técnicas y funciones, aquí hay algunos recursos:

- MDN Web Docs para `String.prototype.toUpperCase()`: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/toUpperCase
- MDN Web Docs para `String.prototype.slice()`: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/slice
- Documentación de Lodash para `_.capitalize`: https://lodash.com/docs/#capitalize