---
title:    "Javascript: Generando números aleatorios"
keywords: ["Javascript"]
---

{{< edit_this_page >}}

# Por qué generar números aleatorios en Javascript

La generación de números aleatorios es una funcionalidad muy útil en la programación. Puede ser necesario generar un número aleatorio para una lotería, un juego, selección aleatoria de ganadores, o simplemente para añadir un elemento de sorpresa en una aplicación. En Javascript, hay varias formas de generar números aleatorios, y en esta publicación te mostraremos cómo hacerlo.

## Cómo hacerlo

Existen varias funciones en Javascript relacionadas con la generación de números aleatorios. Una de las formas más sencillas es la función `Math.random()`. Esta función devuelve un número aleatorio entre 0 (incluido) y 1 (no incluido). A continuación, te mostramos un ejemplo de cómo utilizar esta función:

```Javascript
let numeroAleatorio = Math.random();
console.log(numeroAleatorio);
```

Este código generaría un resultado similar a este:

```Javascript
0.3986785301245775
```

Si queremos generar un número aleatorio entre un rango específico, podemos utilizar la función `Math.floor()` para redondear hacia abajo, y luego multiplicar el resultado por la diferencia entre el máximo y el mínimo del rango, y finalmente sumarle el mínimo. Por ejemplo, si queremos generar un número aleatorio entre 1 y 10, podemos hacer lo siguiente:

```Javascript
let numeroAleatorio = Math.floor(Math.random() * (10 - 1 + 1)) + 1;
console.log(numeroAleatorio);
```

Este código generaría un resultado similar a este:

```Javascript
7
```

## Profundizando

La función `Math.random()` se basa en un algoritmo llamado "Mersenne Twister" para generar números pseudoaleatorios. Esto significa que aunque los números parecen aleatorios, en realidad son generados por una secuencia predecible. Por lo tanto, no se recomienda utilizar esta función para aplicaciones críticas de seguridad.

Js-extend es una biblioteca de Javascript que proporciona funciones más avanzadas para generar números aleatorios, incluidas distribuciones estadísticas y generación de números aleatorios criptográficamente seguros.

## Ver también

- [Documentación de Math.random() en MDN](https://developer.mozilla.org/es/docs/Web/JavaScript/Reference/Global_Objects/Math/random)
- [Ejemplo de generación de números aleatorios con js-extend](https://www.npmjs.com/package/js-extend#randomnumber)