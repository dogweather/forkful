---
title:    "TypeScript: Generando números aleatorios"
keywords: ["TypeScript"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/es/typescript/generating-random-numbers.md"
---

{{< edit_this_page >}}

## ¿Por qué generar números aleatorios en TypeScript?

Generar números aleatorios es una habilidad importante en la programación ya que puede ser útil en una variedad de aplicaciones. Ya sea para crear juegos, simular datos o realizar pruebas, la generación de números aleatorios es una herramienta importante que ahorra tiempo y hace que el código sea más dinámico.

## Cómo hacerlo

En TypeScript, podemos generar números aleatorios utilizando la función `Math.random()`. Esta función devuelve un número decimal aleatorio entre 0 y 1. Para obtener un número entero aleatorio dentro de un rango específico, podemos usar las funciones `Math.floor()` y `Math.ceil()`. Veamos un ejemplo:

```TypeScript
// Generar un número aleatorio entre 1 y 10
let randomNumber = Math.floor(Math.random() * 10) + 1;
console.log(randomNumber); // Output: 7
```

También podemos utilizar la función `Math.random()` en combinación con un bucle `for` para generar múltiples números aleatorios. Por ejemplo:

```TypeScript
// Generar 5 números aleatorios entre 1 y 100
for (let i = 0; i < 5; i++) {
  let randomNumber = Math.floor(Math.random() * 100) + 1;
  console.log(randomNumber); // Output: 53, 28, 99, 15, 72
}
```

## Profundizando

Existen diferentes algoritmos para generar números aleatorios, y algunos pueden ser más eficientes que otros. En TypeScript, la función `Math.random()` utiliza el algoritmo de multiplicador lineal congruencial (LCG), que puede generar números pseudoaleatorios. Esto significa que, aunque parezcan aleatorios a simple vista, los números generados siguen un patrón predecible.

Para aplicaciones que requieren un alto nivel de aleatoriedad, como juegos de azar o sistemas de encriptación, se pueden utilizar otros algoritmos más complejos o generar una semilla a partir de una fuente externa de verdadera aleatoriedad.

## Ver también

- [Documentación de TypeScript sobre la función Math.random()](https://www.typescriptlang.org/docs/handbook/functions.html#return)
- [Más información sobre el algoritmo LCG](https://www.quora.com/What-is-the-linear-congruential-generator-and-what-are-some-good-things-about-it)