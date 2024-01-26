---
title:                "Generando números aleatorios"
date:                  2024-01-20T17:49:18.296367-07:00
model:                 gpt-4-1106-preview
simple_title:         "Generando números aleatorios"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/javascript/generating-random-numbers.md"
---

{{< edit_this_page >}}

## ¿Qué y Por Qué?

Generar números aleatorios en Javascript significa crear valores numéricos impredecibles. Los programadores lo hacen para tareas como juegos, simulaciones y, en seguridad, para generar datos criptográficamente seguros. 

## Cómo hacerlo:

```javascript
// Número aleatorio entre 0 (inclusive) y 1 (exclusivo)
console.log(Math.random());

// Número entero aleatorio entre min (incluido) y max (excluido)
function obtenerEnteroAleatorio(min, max) {
  return Math.floor(Math.random() * (max - min) + min);
}
console.log(obtenerEnteroAleatorio(1, 10));

// Número aleatorio entre min (inclusive) y max (exclusive), flotante.
function obtenerFlotanteAleatorio(min, max) {
  return Math.random() * (max - min) + min;
}
console.log(obtenerFlotanteAleatorio(1.5, 6.5));
```

### Salida de muestra
```
0.4378273649215698
4
3.926467418670654
```

## Profundizando

En los orígenes de Javascript, la función `Math.random()` ya estaba ahí para darnos un número pseudorandom entre 0 y 1. Pero ¿qué significa "pseudorandom"? No son completamente impredecibles como lo sería el azar verdadero, ya que se generan a partir de algoritmos matemáticos.

Para asegurarnos de que los números aleatorios son lo suficientemente buenos para la mayoría de los usos, Javascript implementa generadores con algoritmos complejos. Pero para criptografía, necesitamos algo más seguro y para eso existe `crypto.getRandomValues()`.

Si necesitas generar números aleatorios fuera del estándar 0-1 que nos da `Math.random()`, tienes que hacer un poco de matemática, como se muestra en los ejemplos de "Cómo hacerlo".

## Véase También

- MDN Web Docs ofrece una profunda inmersión en la `Math.random()` y ejemplos de cómo usarla: [MDN Math.random](https://developer.mozilla.org/es/docs/Web/JavaScript/Reference/Global_Objects/Math/random)
- Para más sobre números aleatorios en criptografía con la Web Crypto API: [MDN Crypto.getRandomValues()](https://developer.mozilla.org/es/docs/Web/API/Crypto/getRandomValues)
- Si te interesan los detalles sobre cómo se implementan los generadores de números aleatorios y su calidad, busca el "test de espectro completo de diehard" y la "prueba de entropía".
