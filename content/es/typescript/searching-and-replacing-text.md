---
title:                "TypeScript: Buscando y reemplazando texto"
simple_title:         "Buscando y reemplazando texto"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/typescript/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Por qué

Si eres un programador TypeScript, es muy probable que en algún momento necesites buscar y reemplazar texto dentro de tu código. Esta función puede ahorrar tiempo y esfuerzo, especialmente si estás trabajando en un proyecto grande con una gran cantidad de líneas de código.

## Cómo hacerlo

Para buscar y reemplazar texto en TypeScript, puedes usar la función `replace()` que viene incluida en el lenguaje. Esta función toma dos parámetros: el texto que quieres buscar y el texto por el que lo quieres reemplazar. Por ejemplo, si quieres cambiar todas las instancias de "gato" por "perro" en una cadena de texto, puedes hacerlo de la siguiente manera:

```TypeScript
let cadena = "Mi gato es muy lindo";
let nuevaCadena = cadena.replace("gato", "perro");
console.log(nuevaCadena); // Output: "Mi perro es muy lindo"
```

Si quieres reemplazar todas las instancias de una palabra, debes usar la expresión regular `/g` en el primer parámetro. Por ejemplo:

```TypeScript
let cadena = "Mi gato es muy lindo. Me encantan los gatos!";
let nuevaCadena = cadena.replace(/gato/g, "perro");
console.log(nuevaCadena); // Output: "Mi perro es muy lindo. Me encantan los perros!"
```

## Profundizando

La función `replace()` también puede tomar una expresión regular en lugar de una cadena de texto en el primer parámetro. Esto te permite realizar búsquedas y reemplazos más complejos. Por ejemplo, si quieres reemplazar todas las vocales en una cadena por la letra "x", puedes hacerlo con esta expresión regular: `/[aeiou]/g`.

```TypeScript
let cadena = "Hola mundo";
let nuevaCadena = cadena.replace(/[aeiou]/g, "x");
console.log(nuevaCadena); // Output: "Hxlx mxndx"
```

Ten en cuenta que la función `replace()` devuelve una nueva cadena con todos los cambios aplicados. Si quieres modificar la cadena original, debes asignarla a una nueva variable.

## Ver también

- [Documentación de la función replace() de TypeScript](https://www.typescriptlang.org/docs/handbook/string-methods.html#replace)
- [Tutorial de expresiones regulares en TypeScript](https://codeburst.io/an-introduction-to-regular-expressions-regex-in-typescript-1d3556a7ac5a)