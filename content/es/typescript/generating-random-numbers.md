---
title:                "TypeScript: Generando números aleatorios"
programming_language: "TypeScript"
category:             "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/typescript/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Por qué generar números aleatorios en TypeScript

Generar números aleatorios es una técnica comúnmente utilizada en programación para simular situaciones aleatorias y tomar decisiones al azar. En TypeScript, esta funcionalidad se puede implementar de manera sencilla y efectiva gracias a sus características de tipado estático y robustez.

## Cómo generar números aleatorios en TypeScript

Generar números aleatorios en TypeScript se puede realizar de varias formas. Una manera es utilizando el método `Math.random()` que retorna un número aleatorio entre 0 y 1.

```
TypeScript
let randomNumber = Math.random();
console.log(randomNumber); //output: 0.3412946 
```

También se pueden generar números enteros aleatorios utilizando `Math.floor()` y multiplicando el resultado por un rango deseado.

```
TypeScript
let randomInteger = Math.floor(Math.random() * 10);
console.log(randomInteger) //output: 7
```

Otra forma más específica de generar números aleatorios en TypeScript es utilizando la librería `Random-js`. Primero se debe instalar la librería mediante el comando `npm install random-js` y luego se puede utilizar dentro de un proyecto TypeScript de la siguiente manera:

```
TypeScript
import { Random } from "random-js";
let random = new Random(); //crea una instancia de la clase Random
let randomNumber = random.integer(1, 10); //genera un número aleatorio entre 1 y 10
console.log(randomNumber); //output: 8
```

## Profundizando en la generación de números aleatorios

La generación de números aleatorios puede parecer simple, pero en realidad se basa en algoritmos y técnicas matemáticas complejas. Por ejemplo, el método `Math.random()` utiliza un algoritmo llamado "punto flotante" que genera números pseudoaleatorios. Estos números no son realmente aleatorios, pero para la mayoría de los casos cumplen con su objetivo de simular aleatoriedad.

Un detalle importante a tener en cuenta al generar números aleatorios es que no se debe confiar en ellos para fines de seguridad y criptografía, ya que pueden ser predecibles para alguien con conocimientos avanzados en matemáticas y programación.

## Ver también

- [Documentación de TypeScript: Generación de números aleatorios](https://www.typescriptlang.org/docs/handbook/intro-to-typescript.html#generación-de-números-aleatorios)
- [Artículo sobre generación de números aleatorios en TypeScript](https://medium.com/devtrails/random-number-generation-in-typescript-edc497371e97)
- [Librería Random-js](https://github.com/ckknight/random-js)