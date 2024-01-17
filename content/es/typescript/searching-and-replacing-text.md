---
title:                "Buscando y reemplazando texto"
html_title:           "TypeScript: Buscando y reemplazando texto"
simple_title:         "Buscando y reemplazando texto"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/typescript/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## ¿Qué y por qué?

Buscar y reemplazar texto es una tarea común en la programación, que consiste en encontrar y sustituir una cadena de texto específica dentro de un archivo o documento. Los programadores lo hacen para ahorrar tiempo y para mantener la consistencia en sus códigos.

## Cómo hacerlo:

Para buscar y reemplazar texto en TypeScript, podemos usar el método `replace()` que viene incorporado en su librería de cadenas. Veamos un ejemplo:

```typescript
let textoInicial = '¡Hola, mundo!';
let textoReemplazado = textoInicial.replace('mundo', 'todos');

console.log(textoReemplazado); // Salida: ¡Hola, todos!
```

En este ejemplo, utilizamos el método `replace()` para buscar la palabra "mundo" en la cadena `textoInicial` y reemplazarla por la palabra "todos". El resultado se guarda en la variable `textoReemplazado` y luego lo imprimimos en la consola.

También podemos utilizar expresiones regulares para buscar patrones específicos dentro de una cadena y reemplazarlos. Por ejemplo:

```typescript
let textoInicial = 'Bienvenido a mi blog. En este blog encontrarás artículos sobre programación y tecnología.';

let textoReemplazado = textoInicial.replace(/blog/g, 'sitio web');

console.log(textoReemplazado); // Salida: Bienvenido a mi sitio web. En este sitio web encontrarás artículos sobre programación y tecnología.
```

Aquí, utilizamos la expresión regular `/blog/g` para buscar todas las instancias de la palabra "blog" en el texto y reemplazarlas con la palabra "sitio web".

## Buceo profundo:

Este método de buscar y reemplazar texto no es algo nuevo, ya que ha estado presente en lenguajes de programación desde hace mucho tiempo. En TypeScript, podemos usar también las funciones `replaceAll()` y `replaceAllIgnoreCase()` para reemplazar todas las instancias de una cadena de texto sin importar si están en mayúsculas o minúsculas.

En cuanto a alternativas, podemos usar herramientas externas como el comando `sed` en sistemas Unix o la función `str_replace()` en PHP para realizar acciones similares. Sin embargo, el método `replace()` en TypeScript es una forma sencilla y eficiente de buscar y reemplazar texto en nuestras aplicaciones.

## Ver también:

- [Documentación oficial de TypeScript - Método replace](https://www.typescriptlang.org/docs/handbook/utility-types.html#replace-htm).