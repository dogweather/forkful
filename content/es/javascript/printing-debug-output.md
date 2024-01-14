---
title:    "Javascript: Imprimiendo salida de depuración"
keywords: ["Javascript"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/es/javascript/printing-debug-output.md"
---

{{< edit_this_page >}}

## Por qué

¿Alguna vez te has encontrado con un error en tu código y no tienes ni idea de dónde viene? ¿O has querido ver el valor de una variable en un determinado punto de tu programa? La impresión de salida de depuración (*debug output*) es una herramienta útil para ayudarte a comprender lo que está sucediendo en tu código y a solucionar problemas.

## Cómo hacerlo

Imprimir salida de depuración en Javascript es muy sencillo. Simplemente utiliza el método `console.log()` y pasa como argumento lo que quieras imprimir. Por ejemplo:

```Javascript
var nombre = "María";
console.log("¡Hola, " + nombre + "!");
```

Este código imprimirá en la consola: `¡Hola, María!`. Además de cadenas de texto, también puedes imprimir valores de variables, objetos o cualquier otra forma de datos que desees.

## Profundizando

Además de `console.log()`, existen otras formas de imprimir salida de depuración en Javascript, como `console.info()`, `console.warn()` y `console.error()`. Cada una de ellas tiene un estilo y color diferente en la consola, lo que puede facilitar la identificación de diferentes tipos de información.

Otra opción es utilizar `console.table()` para imprimir datos tabulares en la consola, lo que puede ser especialmente útil para analizar objetos complejos.

También puedes utilizar `console.group()` y `console.groupEnd()` para agrupar varias líneas de salida de depuración, lo que puede ayudar a organizar mejor la información.

Recuerda que siempre debes eliminar o comentar tus líneas de impresión de salida de depuración antes de publicar tu código en producción, ya que estas pueden ralentizar el rendimiento de tu programa.

## Ver también

- [Artículo sobre salida de depuración en Javascript](https://codeburst.io/console-log-the-complete-guide-debugging-javascript-and-node-js-with-console-log-5122cd1c1f9f)
- [Documentación de MDN sobre console.log()](https://developer.mozilla.org/es/docs/Web/API/Console/log)
- [Video tutorial sobre salida de depuración en Javascript](https://www.youtube.com/watch?v=dQw4w9WgXcQ)