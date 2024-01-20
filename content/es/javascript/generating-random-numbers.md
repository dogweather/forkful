---
title:                "Generando números aleatorios"
html_title:           "Arduino: Generando números aleatorios"
simple_title:         "Generando números aleatorios"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/javascript/generating-random-numbers.md"
---

{{< edit_this_page >}}

## ¿Qué y Por qué?

Generar números aleatorios es un proceso por el cual se producen números sin ningún patrón previsible. Los programadores los utilizan para tareas como simular comportamientos naturales, crear variabilidad en sus aplicaciones o para realizar tests con casos aleatorios.

## Cómo hacerlo:

Aquí tienes un código simple para generar un número aleatorio en Javascript entre 0 (incluido) y 1 (excluido).

```Javascript
var aleatorio = Math.random();
console.log(aleatorio);
```

Para generar un número aleatorio dentro de un rango específico, utiliza este cálculo:

```Javascript
var min = 10;
var max = 20;
var aleatorio = Math.floor(Math.random() * (max - min)) + min;
console.log(aleatorio);
```

## Profundización:

Históricamente, JavaScript ha confiado en la función Math.random() para generar números aleatorios. Sin embargo, este método es pseudoaleatorio, lo que significa que utiliza un algoritmo inicial y predecible.

Alternativamente, algunos desarrolladores recurren a la API criptográfica Web Crypto para generar números aleatorios más robustos. Como ejemplo:

```Javascript
var array = new Uint32Array(1);
window.crypto.getRandomValues(array);
console.log(array[0]);
```
Este método es generalmente seguro pero carece de compatibilidad con navegadores más antiguos.

## Ver también: 

[MDN Web Docs - Math.random()](https://developer.mozilla.org/es/docs/Web/JavaScript/Referencia/Objetos_globales/Math/random)

[MDN Web Docs - Web Crypto API](https://developer.mozilla.org/es/docs/Web/API/Window/crypto)