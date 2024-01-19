---
title:                "Eliminando caracteres que coinciden con un patrón"
html_title:           "Elixir: Eliminando caracteres que coinciden con un patrón"
simple_title:         "Eliminando caracteres que coinciden con un patrón"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/typescript/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

# Eliminación de caracteres que coinciden con un patrón en TypeScript

## ¿Qué y por qué?

Eliminar caracteres que coinciden con un patrón es la tarea de buscar y remover caracteres específicos dentro de una cadena de texto. Los programadores hacen esto para manipular o limpiar datos efectivamente.

## Como Hacerlo:

Aquí veremos cómo eliminar todas las instancias de una letra específica (por ejemplo, 'a') de una cadena usando funciones incorporadas en TypeScript. Usa el método `replace()` con una expresión regular.

```typescript
let str = 'banana';
let newStr = str.replace(/a/g, '');
console.log(newStr); // Outputs: 'bnn'
```

## Análisis en Profundidad

La eliminación de caracteres que coinciden con un patrón ha sido una técnica utilizada durante décadas en la programación y normalmente se realiza mediante expresiones regulares, una poderosa herramienta en el procesamiento de texto.

Aunque hemos usado el método `replace()` en el ejemplo, existen otras maneras de lograr lo mismo, como bucles o métodos de cadenas especializados en lenguajes como Python (`str.translate()`) o Ruby (`str.tr()`).

La implementación del método `replace()` en JavaScript (y por extensión TypeScript) utiliza un algoritmo de coincidencia y reemplazo muy rápido, haciéndolo muy eficiente para tareas de procesamiento masivo de texto.

## Ver También

Para una comprensión más profunda de las expresiones regulares y su uso en TypeScript, consulte estas fuentes:

1. [Expresiones regulares en TypeScript - Mozilla Developer Network](https://developer.mozilla.org/es/docs/Web/JavaScript/Guide/Regular_Expressions)
2. [Método replace() - TypeScript Handbook](https://www.typescriptlang.org/docs/handbook/utility-types.html)