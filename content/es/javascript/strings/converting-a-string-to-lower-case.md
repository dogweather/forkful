---
date: 2024-01-20 17:38:53.186882-07:00
description: "C\xF3mo hacerlo: Aqu\xED una muestra de c\xF3mo convertir una cadena\
  \ a min\xFAsculas en JavaScript. Solo se necesita `toLowerCase`."
lastmod: '2024-03-13T22:44:59.445930-06:00'
model: gpt-4-1106-preview
summary: "Aqu\xED una muestra de c\xF3mo convertir una cadena a min\xFAsculas en JavaScript."
title: "Conversi\xF3n de una cadena de texto a min\xFAsculas"
weight: 4
---

## Cómo hacerlo:
Aquí una muestra de cómo convertir una cadena a minúsculas en JavaScript. Solo se necesita `toLowerCase`.

```javascript
let frase = "¡Hola, Me Llamo Javascript!";
let fraseEnMinusculas = frase.toLowerCase();

console.log(fraseEnMinusculas);
// Output: "¡hola, me llamo javascript!"
```

Si tienes algo más complejo, usa `toLocaleLowerCase()` para considerar reglas específicas de idioma.

```javascript
let saludoTurco = "MERHABA";
console.log(saludoTurco.toLocaleLowerCase('tr-TR'));
// Output: "merhaba"
```

## Inmersión Profunda
JavaScript ofrece `toLowerCase()` desde su versión inicial. Este método es parte del estándar ECMAScript y se utiliza en todas las cadenas de texto. Es sencillo y, a menos que necesites reglas específicas de localización, es lo que usarás la mayoría de veces.

Alternativas incluyen `toLocaleLowerCase()`, que recomendamos para soportar caracteres únicos de ciertas lenguas. También existían métodos antiguos como `toLowerCase` en prototipos de objetos que heredaran de `String`, pero esos han sido reemplazados y no son recomendables.

En cuanto a implementación, `toLowerCase()` no cambia la cadena original, sino que devuelve una nueva. Es por eso que tienes que almacenar el resultado en una nueva variable si quieres usarlo.

## Véase También:
- Documentación de Mozilla MDN sobre `toLowerCase()` para más ejemplos y detalles: [MDN toLowerCase()](https://developer.mozilla.org/es/docs/Web/JavaScript/Reference/Global_Objects/String/toLowerCase)
- Comparación de `toLowerCase()` y `toLocaleLowerCase()`: [MDN toLocaleLowerCase()](https://developer.mozilla.org/es/docs/Web/JavaScript/Reference/Global_Objects/String/toLocaleLowerCase)
- Una guía sobre Unicode y JavaScript: [JavaScript y Unicode](https://flaviocopes.com/javascript-unicode/)
