---
title:                "Generando números aleatorios"
html_title:           "Arduino: Generando números aleatorios"
simple_title:         "Generando números aleatorios"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/typescript/generating-random-numbers.md"
---

{{< edit_this_page >}}

## ¿Qué y por qué?

Generar números aleatorios es el proceso de producir números que no pueden predecirse mejor que por una casualidad, es decir, que parecen carecer de cualquier patrón. Los programadores necesitan generar números aleatorios para simular eventos aleatorios, saltear elementos al azar, hacer llamadas API a intervalos aleatorios, etc.

## Cómo hacer:

Para generar un número aleatorio en TypeScript, puedes utilizar el método `Math.random()`. Aquí está el código de muestra y la salida 

```TypeScript
let numeroAleatorio = Math.random();
console.log(numeroAleatorio);
```
Salida:

```TypeScript 
0.5138176345648667
```
Para un rango específicos de números aleatorios, multiplica con el número más alto requerido y luego suma el número más bajo al resultado.

```TypeScript
function obtenerAleatorio(min: number, max: number): number {
  let rango = max - min + 1;
  return Math.floor(Math.random() * rango) + min;
}
console.log(obtenerAleatorio(1, 6));  //generar un número aleatorio de 1 a 6
```
Salida:

```TypeScript
4
```
## Conociendo más:

Historia: Los números aleatorios han sido útiles para las simulaciones desde los primeros días de los ordenadores. Las primeras simulaciones de Montecarlo utilizaban tablas de números aleatorios para generar datos.

Alternativas: Además del método `Math.random()`, hay otros métodos para generar números aleatorios. Puedes utilizar una librería como CryptoJS para generar números aleatorios criptográficamente seguros. 

Implementación: `Math.random()` genera un número entre 0 y 1. Pero no es criptográficamente seguro, lo que significa que no debería ser utilizado para funciones relacionadas con la seguridad en la web. Si necesitas un número aleatorio criptográficamente seguro en TypeScript, tendrías que generar uno con el API de Criptografía Web.

```TypeScript
let array = new Uint32Array(1);
window.crypto.getRandomValues(array);
console.log(array[0]);
```
Salida:

```TypeScript
2893567456
```

## Ver también:

1. Artículo en MDN sobre la API de Math en JavaScript: https://developer.mozilla.org/es/docs/Web/JavaScript/Reference/Global_Objects/Math

2. Documentación oficial de TypeScript en español: https://www.typescriptlang.org/docs/handbook/intro.html

3. Introducción a Monte Carlo: http://www.leg.ufpr.br/~paulojus/CE068/node34.html