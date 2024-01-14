---
title:                "TypeScript: Generando números aleatorios"
simple_title:         "Generando números aleatorios"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/typescript/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Por qué generar números aleatorios en TypeScript

Generar números aleatorios es una técnica común utilizada en programación para simular situaciones de azar o para realizar pruebas. En TypeScript, esta técnica puede ser implementada utilizando la función `Math.random()`.

## Cómo generar números aleatorios en TypeScript

Para generar un número aleatorio en TypeScript, simplemente debemos utilizar la función `Math.random()` y multiplicarla por el rango de números deseado. Por ejemplo, si queremos generar un número entre 1 y 10, podemos utilizar el siguiente código:

```TypeScript
let numeroAleatorio = Math.random() * 10;
console.log(numeroAleatorio);
```
Este código generará un número aleatorio entre 0 y 10, incluyendo decimales. Para obtener un número entero, podemos utilizar la función `Math.floor()` para redondear hacia abajo el resultado de `Math.random()`:

```TypeScript 
let numeroAleatorio = Math.floor(Math.random() * 10);
console.log(numeroAleatorio);
```

También podemos especificar un rango personalizado utilizando la diferencia entre el número máximo y el mínimo en lugar de solo el máximo:

```TypeScript
let min = 20;
let max = 50;
let numeroAleatorio = Math.floor(Math.random() * (max - min + 1)) + min;
console.log(numeroAleatorio);
```

Este código generará un número aleatorio entre 20 y 50.

## Profundizando en la generación de números aleatorios

Aunque la función `Math.random()` es bastante simple de utilizar, es importante tener en cuenta que no es verdaderamente aleatoria. En realidad, esta función utiliza un algoritmo matemático para generar una secuencia de números pseudorandom, lo que significa que los números aparentan ser aleatorios, pero en realidad son predecibles.

Si necesitamos una generación de números más precisa, podemos utilizar bibliotecas externas como [random-js](https://github.com/ckknight/random-js) o [crypto](https://nodejs.org/api/crypto.html) para generar números verdaderamente aleatorios.

## También te puede interesar

- [El arte de generar números aleatorios en TypeScript](https://medium.com/@lalomanu/el-arte-de-generar-n%C3%BAmeros-aleatorios-en-typescript-e5923e902b25)
- [Generación de números aleatorios en TypeScript: mitos y verdades](https://dev.to/programarivm/generacion-de-numeros-aleatorios-en-typescript-mitos-y-verdades-374k)
- [Cómo utilizar la función Math en TypeScript](https://docs.microsoft.com/es-es/previous-versions/visualstudio/typescript-language-reference/whas6h24%28v%3dvs.140%29)