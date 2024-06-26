---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:05:34.329362-07:00
description: "C\xF3mo hacerlo: En JavaScript, no existe un m\xE9todo incorporado para\
  \ capitalizar cadenas directamente, pero es f\xE1cil de implementar utilizando m\xE9\
  todos\u2026"
lastmod: '2024-03-13T22:44:59.441953-06:00'
model: gpt-4-0125-preview
summary: "En JavaScript, no existe un m\xE9todo incorporado para capitalizar cadenas\
  \ directamente, pero es f\xE1cil de implementar utilizando m\xE9todos b\xE1sicos\
  \ de manipulaci\xF3n de cadenas."
title: Capitalizando una cadena de texto
weight: 2
---

## Cómo hacerlo:
En JavaScript, no existe un método incorporado para capitalizar cadenas directamente, pero es fácil de implementar utilizando métodos básicos de manipulación de cadenas.

### Usando JavaScript Estándar
```javascript
function capitalize(str) {
  if (!str) return '';
  return str.charAt(0).toUpperCase() + str.slice(1);
}

console.log(capitalize('hello world')); // Salida: "Hello world"
```

### Versión ES6
Con los literales de plantilla de ES6, la función se puede escribir de una manera más sucinta:
```javascript
const capitalize = (str) => !str ? '' : `${str[0].toUpperCase()}${str.slice(1)}`;

console.log(capitalize('hello ES6')); // Salida: "Hello ES6"
```

### Usando Lodash
Lodash es una popular biblioteca de utilidades de terceros que ofrece una amplia gama de funciones para manipular y trabajar con valores de JavaScript, incluyendo cadenas. Para capitalizar una cadena usando Lodash:
```javascript
// Primero, instala lodash si aún no lo has hecho: npm install lodash
const _ = require('lodash');

console.log(_.capitalize('EJEMPLO LODASH')); // Salida: "Lodash ejemplo"
```
_Notar cómo Lodash no solo capitaliza la primera letra sino que también convierte el resto de la cadena a minúsculas, lo cual difiere ligeramente de la implementación en JavaScript puro._

### Usando CSS (Solo para Fines de Visualización)
Si el objetivo es capitalizar texto para mostrarlo en la UI, se puede usar CSS:
```css
.capitalize {
  text-transform: capitalize;
}
```
```html
<div class="capitalize">hola css</div> <!-- Se muestra como "Hola css" -->
```
**Nota:** Este método cambia la manera en que el texto aparece en la página web sin alterar la cadena en sí misma en JavaScript.
