---
title:                "Generando números aleatorios"
html_title:           "TypeScript: Generando números aleatorios"
simple_title:         "Generando números aleatorios"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/typescript/generating-random-numbers.md"
---

{{< edit_this_page >}}

## ¿Qué & Por qué?

Generar números aleatorios es una técnica comúnmente utilizada por los programadores para generar valores pseudoaleatorios en un rango específico. Esto puede ser útil para simular juegos, pruebas aleatorias y muchas otras aplicaciones en las que se requiere un elemento de azar.

## Cómo:

Aquí hay dos formas de generar números aleatorios en TypeScript:

### Utilizando la función Math.random ()
```TypeScript
let numero = Math.floor(Math.random() * 100) + 1;
console.log(numero); // output: un número aleatorio entre 1 y 100
```

Este método utiliza la función Math.random () de JavaScript para generar un número decimal entre 0 y 1. Luego, se multiplica por el rango deseado y se usa la función Math.floor () para redondear el resultado a un número entero.

### Utilizando la librería random-js
```TypeScript
import { Random } from 'random-js';

// crear una instancia de Random
const random = new Random();

let numero = random.integer(1, 100);
console.log(numero); // output: un número aleatorio entre 1 y 100
```

La librería [random-js](https://github.com/ckknight/random-js) proporciona una forma más avanzada de generar números aleatorios en TypeScript. Además de generar números enteros en un rango específico, también permite generar arrays, especificar el generador de números aleatorios y repetir la secuencia.

## Inmersión Profunda

### Contexto Histórico
La generación de números aleatorios ha sido un tema de estudio y debate desde los primeros días de la informática. En la década de 1940, John von Neumann y Stanislaw Ulam desarrollaron el primer generador de números aleatorios utilizando una combinación de hardware y software. Desde entonces, ha habido muchas técnicas y algoritmos para generar números pseudoaleatorios.

### Alternativas
Además de las opciones mencionadas, TypeScript también ofrece la posibilidad de utilizar un generador de números aleatorios externo, como [Random.org](https://www.random.org/), que utiliza fuentes de datos aleatorios del mundo real.

### Detalles de la implementación
La función Math.random () utiliza el generador de números aleatorios subyacente del sistema operativo o del navegador, mientras que la librería random-js tiene su propio generador basado en algoritmos matemáticos. Ambos se basan en la semilla inicial proporcionada y, por lo tanto, pueden ser replicados para obtener la misma secuencia de números aleatorios.

## Ver También

- [Documentación de Math.random () en MDN](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Math/random)
- [Random.js en GitHub](https://github.com/ckknight/random-js)