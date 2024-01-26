---
title:                "Generando números aleatorios"
date:                  2024-01-20T17:49:58.529792-07:00
model:                 gpt-4-1106-preview
simple_title:         "Generando números aleatorios"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/typescript/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Qué y Por Qué?

Generar números aleatorios es el acto de crear números que no tienen un patrón predecible. Los programadores utilizan números aleatorios para todo: desde juegos hasta simulaciones, pasando por la encriptación y hasta el manejo de sesiones en aplicaciones web.

## Cómo:

Generar un número aleatorio entre 0 y 1:

```TypeScript
let random_num = Math.random();
console.log(random_num);
```

Pero, ¿y si quieres un número más grande? Digamos, entre 1 y 100:

```TypeScript
let random_entre_1_y_100 = Math.floor(Math.random() * 100) + 1;
console.log(random_entre_1_y_100);
```

Para un rango personalizado, como entre 15 y 80:

```TypeScript
function obtenerAleatorio(min: number, max: number): number {
  return Math.floor(Math.random() * (max - min + 1)) + min;
}

console.log(obtenerAleatorio(15, 80));
```

## Deep Dive

Históricamente, generar números aleatorios en computadoras no es aleatorio en el sentido estricto, sino que es "pseudoaleatorio". Trabajamos con algoritmos determinísticos que simulan aleatoriedad. Desde los sencillos, como el generador de congruencia lineal, hasta los más avanzados, como los algoritmos Mersenne Twister.

Una alternativa moderna es el Web Cryptography API, que proporciona una fuente más segura de aleatoriedad y que es criptográficamente segura:

```TypeScript
window.crypto.getRandomValues(new Uint32Array(1))[0];
```

En cuanto a implementación, siempre es importante considerar la distribución de los números generados. Para usos que requieren una seguridad más fuerte, tales como la criptografía, siempre hay que optar por generadores seguros a nivel criptográfico, en lugar de `Math.random()`.

## Ver También

- [MDN Web Docs - Math.random()](https://developer.mozilla.org/es/docs/Web/JavaScript/Reference/Global_Objects/Math/random)
- [MDN Web Docs - Crypto.getRandomValues()](https://developer.mozilla.org/es/docs/Web/API/Crypto/getRandomValues)
- [Stack Overflow - Generación de números aleatorios en TypeScript](https://es.stackoverflow.com/questions/tagged/typescript+random)
