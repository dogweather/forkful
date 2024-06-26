---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:06:32.285288-07:00
description: "C\xF3mo hacerlo: TypeScript, al ser un superconjunto de JavaScript,\
  \ permite varios m\xE9todos para capitalizar cadenas, que van desde enfoques puros\
  \ de\u2026"
lastmod: '2024-03-13T22:44:58.781956-06:00'
model: gpt-4-0125-preview
summary: "TypeScript, al ser un superconjunto de JavaScript, permite varios m\xE9\
  todos para capitalizar cadenas, que van desde enfoques puros de JavaScript hasta\
  \ la utilizaci\xF3n de bibliotecas de terceros para casos de uso m\xE1s complejos\
  \ o espec\xEDficos."
title: Capitalizando una cadena de texto
weight: 2
---

## Cómo hacerlo:
TypeScript, al ser un superconjunto de JavaScript, permite varios métodos para capitalizar cadenas, que van desde enfoques puros de JavaScript hasta la utilización de bibliotecas de terceros para casos de uso más complejos o específicos.

**Enfoque de JavaScript Puro:**

```typescript
function capitalize(str: string): string {
  return str.charAt(0).toUpperCase() + str.slice(1);
}

// Salida de muestra:
console.log(capitalize('hello TypeScript!')); // 'Hello TypeScript!'
```

Este método es directo y se basa en el método `charAt()` para acceder al primer carácter de la cadena y en `toUpperCase()` para convertirlo a mayúsculas. El método `slice(1)` luego recupera el resto de la cadena, dejándola sin cambios.

**Usando la Biblioteca Lodash:**

Para proyectos que ya están utilizando la biblioteca [Lodash](https://lodash.com/), puedes utilizar su función `_.capitalize` para lograr el mismo resultado con menos código repetitivo.

Primero, instala Lodash:

```bash
npm install lodash
```

Luego, úsalo en tu archivo TypeScript:

```typescript
import * as _ from 'lodash';

// Salida de muestra:
console.log(_.capitalize('hello TypeScript!')); // 'Hello typescript!'
```

Nota: El método `_.capitalize` de Lodash convierte el resto de la cadena a minúsculas, lo que podría no ser siempre lo que quieres.

**Usando una Expresión Regular:**

Una expresión regular puede proporcionar una manera concisa de capitalizar la primera letra de una cadena, especialmente si necesitas capitalizar la primera letra de cada palabra en una cadena.

```typescript
function capitalizeWords(str: string): string {
  return str.replace(/\b\w/g, char => char.toUpperCase());
}

// Salida de muestra:
console.log(capitalizeWords('hello typescript world!')); // 'Hello Typescript World!'
```

Este método utiliza la función `replace()` para buscar cualquier límite de palabra seguido de un carácter alfanumérico (`\b\w`), capitalizando cada coincidencia. Es particularmente útil para títulos o encabezados.
