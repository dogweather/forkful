---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:17:12.216046-07:00
description: "C\xF3mo hacerlo: Para empezar, puedes crear un patr\xF3n de regex simple\
  \ y usarlo para encontrar coincidencias en una cadena. Aqu\xED, encontraremos la\
  \ palabra\u2026"
lastmod: '2024-03-13T22:44:59.448879-06:00'
model: gpt-4-0125-preview
summary: "Para empezar, puedes crear un patr\xF3n de regex simple y usarlo para encontrar\
  \ coincidencias en una cadena."
title: Usando expresiones regulares
weight: 11
---

## Cómo hacerlo:


### Coincidencia Básica
Para empezar, puedes crear un patrón de regex simple y usarlo para encontrar coincidencias en una cadena. Aquí, encontraremos la palabra "código":

```javascript
const str = "Me encanta programar en JavaScript.";
const pattern = /código/;
const result = pattern.test(str);
console.log(result); // true
```

### Usando `String.prototype.match()`
Para recuperar un arreglo de coincidencias:

```javascript
const matches = str.match(/código/);
console.log(matches[0]); // "código"
console.log(matches.index); // 10
```

### Búsqueda Global
Para encontrar todas las coincidencias, usa la bandera `g`:

```javascript
const globalMatches = str.match(/o/g);
console.log(globalMatches); // ["o", "o", "o"]
```

### Coincidencia sin Importar Mayúsculas o Minúsculas
La bandera `i` ignora mayúsculas y minúsculas:

```javascript
const caseInsensitiveMatch = "JavaScript es divertido".match(/javascript/i);
console.log(caseInsensitiveMatch[0]); // "JavaScript"
```

### Reemplazando Texto
Usa `String.prototype.replace()` para reemplazar partes de la cadena:

```javascript
const newStr = "JavaScript es divertido".replace(/divertido/, "asombroso");
console.log(newStr); // "JavaScript es asombroso"
```

### Usando Grupos
Los grupos pueden capturar partes del patrón:

```javascript
const groupedPattern = /(\w+) es (\w+)/;
const replaceWithGroups = "JavaScript es divertido".replace(groupedPattern, "$2 es $1");
console.log(replaceWithGroups); // "divertido es JavaScript"
```

### Librerías de Terceros
Aunque las capacidades de regex integradas en JavaScript son poderosas, algunas tareas podrían simplificarse con librerías como `XRegExp`. Ofrece sintaxis y banderas adicionales, haciendo patrones complejos más legibles:

```javascript
// Ejemplo de la librería XRegExp
const XRegExp = require('xregexp');
const str = "Los gatos son fantásticos.";
const unicodeWordMatch = XRegExp.match(str, XRegExp('\\p{L}+'), 'all');
console.log(unicodeWordMatch); // ["Los", "gatos", "son", "fantásticos"]
```

Este fragmento demuestra el uso de `XRegExp` para coincidir todas las palabras Unicode en una cadena, mostrando la capacidad de la librería para manejar conjuntos de caracteres extendidos más allá de las capacidades integradas en JavaScript.
