---
title:                "Generando números aleatorios"
html_title:           "Javascript: Generando números aleatorios"
simple_title:         "Generando números aleatorios"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/javascript/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Qué y por qué?

Generar números aleatorios es una técnica comúnmente utilizada por los programadores para generar valores aleatorios en un programa. Esto puede servir para simular situaciones de la vida real, generar datos de prueba o mejorar la aleatoriedad de un juego. 

## Cómo:

```Javascript 
// Ejemplo 1: Generar un número aleatorio entre 1 y 10
let randomNumber = Math.floor(Math.random() * 10) + 1;
console.log(randomNumber); // Output: 7

// Ejemplo 2: Generar un número aleatorio entre 50 y 100
let min = 50;
let max = 100;
let random = Math.floor(Math.random() * (max - min + 1) + min);
console.log(random); // Output: 87
```
Código de ejemplo: https://codepen.io/manuquerty/full/LYpzLxB

## Profundizando:

La generación de números aleatorios es un concepto ampliamente utilizado en la informática, que tiene sus raíces en el campo de la probabilidad y la estadística. Antiguamente, los programadores utilizaban algoritmos que no eran realmente aleatorios, sino pseudoaleatorios, es decir, que siguen un patrón predecible. Sin embargo, hoy en día, los lenguajes de programación modernos cuentan con funciones y librerías que permiten generar números verdaderamente aleatorios.

Existen también otras técnicas para generar números aleatorios, como usar variables de tiempo o datos externos como fuente de entropía. Sin embargo, la función Math.random() en JavaScript es la forma más sencilla y común de obtener valores aleatorios.

## Vea también:
- Documentación oficial de la función Math.random() en MDN: https://developer.mozilla.org/es/docs/Web/JavaScript/Reference/Global_Objects/Math/random
- Más ejemplos de generación de números aleatorios en JavaScript: https://stackabuse.com/javascript-generate-random-string/