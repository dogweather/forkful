---
date: 2024-01-27 20:34:25.002565-07:00
description: "C\xF3mo hacerlo: La manera m\xE1s directa de generar un n\xFAmero aleatorio\
  \ en JavaScript es usar `Math.random()`. Esta funci\xF3n retorna un n\xFAmero flotante,\
  \ pseudo-\u2026"
lastmod: '2024-03-13T22:44:59.454704-06:00'
model: gpt-4-0125-preview
summary: "La manera m\xE1s directa de generar un n\xFAmero aleatorio en JavaScript\
  \ es usar `Math.random()`."
title: "Generaci\xF3n de n\xFAmeros aleatorios"
weight: 12
---

## Cómo hacerlo:


### Generación Básica de Números Aleatorios
La manera más directa de generar un número aleatorio en JavaScript es usar `Math.random()`. Esta función retorna un número flotante, pseudo-aleatorio en el rango de 0 (inclusive) a 1 (exclusivo).

```javascript
let randomNumber = Math.random();
console.log(randomNumber);
```

### Generando un Número Aleatorio Dentro de un Rango
A menudo, querrás un entero aleatorio dentro de un rango específico. Esto se puede lograr escalando y redondeando la salida de `Math.random()`.

```javascript
function getRandomInt(min, max) {
  min = Math.ceil(min);
  max = Math.floor(max);
  return Math.floor(Math.random() * (max - min + 1)) + min;
}

console.log(getRandomInt(1, 100));
```

### Números Aleatorios Criptográficamente Seguros
Para aplicaciones que requieren un grado mayor de aleatoriedad (p.ej., operaciones criptográficas), se puede utilizar el método `crypto.getRandomValues()`. Esto proporciona aleatoriedad criptográfica, a diferencia de los números pseudo-aleatorios generados por `Math.random()`.

```javascript
(function generateSecureRandom() {
  let array = new Uint32Array(1);
  window.crypto.getRandomValues(array);
  console.log(array[0]);
})();
```

## Profundización
Históricamente, la generación de números aleatorios en JavaScript dependía únicamente de la función `Math.random()`. Aunque conveniente para la mayoría de los casos de uso casuales, su algoritmo, típicamente una variante de un generador de números pseudoaleatorios (PRNG) como Mersenne Twister, no proporciona seguridad criptográfica.

La introducción de la API de Criptografía Web trajo el método `crypto.getRandomValues()`, ofreciendo una manera de generar números que son mucho menos predecibles y adecuados para aplicaciones sensibles a la seguridad. Este método aprovecha las fuentes de aleatoriedad del sistema operativo subyacente, como `/dev/random` en Unix/Linux, las cuales son más robustas y adecuadas para operaciones criptográficas.

Es crucial elegir el método adecuado para la tarea en cuestión. `Math.random()` es suficiente para necesidades básicas como juegos simples, animaciones, o cualquier caso donde la calidad de la aleatoriedad no es crítica. Sin embargo, para características de seguridad, como tokens de restablecimiento de contraseña o cualquier operación criptográfica, `crypto.getRandomValues()` es la mejor opción debido a su superior calidad de aleatoriedad.

Notablemente, `Math.random()` genera números con un sesgo conocido en la mayoría de las implementaciones, lo que significa que algunos números son más propensos a ocurrir que otros. Aunque este sesgo es mínimo y a menudo imperceptible para aplicaciones generales, descalifica a `Math.random()` para ser usado en cualquier contexto criptográfico o aplicaciones donde la equidad es crítica, como los juegos de azar en línea.

En conclusión, mientras que las funciones incorporadas de JavaScript para generar números aleatorios cubren un amplio rango de necesidades, entender las diferencias y limitaciones de cada método es esencial para su aplicación apropiada.
