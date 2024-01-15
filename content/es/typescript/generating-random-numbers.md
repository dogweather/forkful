---
title:                "Generación de números aleatorios"
html_title:           "TypeScript: Generación de números aleatorios"
simple_title:         "Generación de números aleatorios"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/typescript/generating-random-numbers.md"
---

{{< edit_this_page >}}

## ¿Por qué generar números aleatorios?

Generar números aleatorios es una tarea común en la programación, especialmente en juegos, simulaciones y pruebas de rendimiento. Al utilizar números aleatorios, podemos simular escenarios realistas y aleatorizar nuestras aplicaciones para obtener resultados más interesantes y precisos.

## Cómo hacerlo en TypeScript

Para generar números aleatorios en TypeScript, utilizaremos la función `Math.random()`. Esta función devuelve un número aleatorio entre 0 (incluido) y 1 (excluido) como un decimal.

```
TypeScript
// Generar un número aleatorio entre 0 y 1
let randomNumber = Math.random();
console.log(randomNumber); // 0.237982
```

Si queremos un número aleatorio dentro de un rango específico, podemos multiplicar `Math.random()` por la diferencia entre el número más alto y el más bajo, y luego sumarle el número más bajo. Por ejemplo, si queremos un número aleatorio entre 1 y 10:

```
TypeScript
// Generar un número aleatorio entre 1 y 10
let randomNumber = Math.random() * 9 + 1;
console.log(randomNumber); // 7.591102
```

También podemos usar `Math.floor()` para redondear el número aleatorio a un número entero:

```
TypeScript
// Generar un número entero aleatorio entre 1 y 10
let randomNumber = Math.floor(Math.random() * 10 + 1);
console.log(randomNumber); // 8
```

## Inmersión profunda

Ahora, profundicemos en cómo funciona `Math.random()` en TypeScript. La función se basa en el generador de números pseudoaleatorios del sistema operativo, que utiliza una semilla para generar números. Una semilla es un valor inicial que se utiliza como punto de partida para generar los números. En TypeScript, esta semilla es generada automáticamente por el sistema operativo y no se puede cambiar.

Cada vez que se utiliza `Math.random()`, el generador de números pseudoaleatorios se actualiza utilizando la fórmula `value = (value * multiplier + increment) % modulus`. Esto significa que cada vez que se ejecuta el código, se obtendrá un número diferente.

Una cosa importante a tener en cuenta es que `Math.random()` no es realmente aleatorio, ya que se basa en una fórmula matemática. Esto significa que si conocemos la semilla del generador, podemos predecir qué numero se generará a continuación. Por lo tanto, es importante utilizar esta función con precaución y no en situaciones donde se necesite verdadera aleatoriedad, como en juegos de azar.

## Ver también

- Documentación de `Math.random()` en TypeScript: https://developer.mozilla.org/es/docs/Web/JavaScript/Referencia/Objetos_globales/Math/random
- Generación de números aleatorios en TypeScript: https://www.typescriptlang.org/docs/handbook/2/types-and-variables.html#randomnumber-generation