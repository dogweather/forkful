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

## ¿Por qué generar números aleatorios?

Generar números aleatorios es una técnica muy útil en programación, ya que permite crear aplicaciones dinámicas y divertidas. Además, puede ser utilizado en juegos, sorteos y algoritmos de optimización.

## Cómo generar números aleatorios en Javascript

Para generar números aleatorios en Javascript, podemos utilizar la función ```Math.random()```. Esta función devuelve un número decimal entre 0 y 1. Luego, podemos multiplicarlo por el rango de números que queremos obtener y sumarle el número mínimo. Veamos un ejemplo:

```Javascript
// Generar un número aleatorio entre 1 y 10
let numeroAleatorio = Math.random() * 10 + 1;
console.log(numeroAleatorio); // 4.7824312
```

También podemos usar la función ```Math.floor()``` para redondear el número hacia abajo y obtener un número entero:

```Javascript
// Generar un número entero aleatorio entre 1 y 10
let numeroAleatorio = Math.floor(Math.random() * 10 + 1);
console.log(numeroAleatorio); // 4
```

Incluso, podemos utilizar la función ```Math.round()``` para redondear el número hacia el número entero más cercano. Esto puede ser útil si queremos un rango de números con decimales:

```Javascript
// Generar un número aleatorio entre 1 y 10 con un decimal
let numeroAleatorio = Math.round(Math.random() * 10 + 1);
console.log(numeroAleatorio); // 7.5
```

## Profundizando en la generación de números aleatorios

La función ```Math.random()``` utiliza un algoritmo matemático para generar los números aleatorios. Sin embargo, este método no es completamente aleatorio, ya que siempre sigue un patrón predecible. Para obtener una mayor aleatoriedad, se pueden utilizar librerías externas como ```Chance.js``` o ```Random.js```.

Por otro lado, es importante tener en cuenta que los números generados por estas funciones no son realmente aleatorios, sino pseudoaleatorios. Esto significa que siguen un patrón determinado, pero para fines prácticos se consideran aleatorios.

## Ver también

- [Documentación de Math.random() en MDN](https://developer.mozilla.org/es/docs/Web/JavaScript/Reference/Global_Objects/Math/random)
- [Librería Chance.js](https://chancejs.com/)
- [Librería Random.js](https://random.js.org/)