---
title:                "Buscando y reemplazando texto"
html_title:           "C: Buscando y reemplazando texto"
simple_title:         "Buscando y reemplazando texto"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/typescript/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

# Búsqueda y Reemplazo de Texto en TypeScript

## ¿Qué y Por Qué?

La búsqueda y el reemplazo de texto son operaciones comunes para manipular cadenas de texto en la programación. Los programadores realizan estas tareas para modificar, refactorizar código, limpiar datos y más. 

## ¿Cómo hacer?

En TypeScript, puedes buscar y reemplazar texto utilizando el método `replace()` siguiendo los siguientes pasos:

```TypeScript
let texto = '¡Hola, mundo!';

texto = texto.replace('mundo', 'todos');

console.log(texto); // Salida: '¡Hola, todos!'
```
Este ejemplo busca la palabra "mundo" en la cadena de texto y la reemplaza por "todos". Los métodos de cadena de texto son sensitivos a mayúsculas y minúsculas, si buscas "Mundo" en lugar de "mundo", no habrá coincidencias.

## Más Profundo

**Contexto Histórico**: El método `replace()` ha existido en JavaScript desde su primera versión, ECMAScript 1. TypeScript, siendo un superconjunto de JavaScript, lo heredó naturalmente.

**Alternativas**: Además de `replace()`, también puedes usar `split()` y `join()` para reemplazar texto. Este enfoque puede ser útil cuando necesitas reemplazar todas ocurrencias de una subcadena.

```TypeScript
let texto = '¡Adiós, mundo! ¡Adiós, mundo!';

texto = texto.split('mundo').join('todos');

console.log(texto); // Salida: '¡Adiós, todos! ¡Adiós, todos!'
```
**Detalles de Implementación**: Notarás que para `replace()`, sólo la primera coincidencia es reemplazada. Si quieres reemplazar todas las coincidencias, puedes usar una expresión regular con la bandera 'g' (global).

```TypeScript
let texto = '¡Adiós, mundo! ¡Adiós, mundo!';

texto = texto.replace(/mundo/g, 'todos');

console.log(texto); // Salida: '¡Adiós, todos! ¡Adiós, todos!'
```

## Ver También

1. [Documentación Oficial de TypeScript](https://www.typescriptlang.org/docs/)
2. [Guía de JavaScript para manipulación de cadenas](https://developer.mozilla.org/es/docs/Web/JavaScript/Guide/Regular_Expressions)
3. [Explicación detallada de replace()](https://www.w3schools.com/jsref/jsref_replace.asp)