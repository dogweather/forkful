---
title:    "Javascript: Generando números aleatorios"
keywords: ["Javascript"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/es/javascript/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Por qué generar números aleatorios en Javascript?

Generar números aleatorios es una tarea común en la programación, especialmente en aplicaciones de juegos y en la creación de algoritmos de prueba y error. En Javascript, existen varias formas de generar números aleatorios que pueden resultar útiles en diferentes situaciones.

## Cómo generar números aleatorios en Javascript

Hay varias formas de generar números aleatorios en Javascript, y cada una tiene sus propias ventajas y desventajas. Aquí te mostramos tres métodos básicos que puedes utilizar en tus proyectos.

- **Math.random():** Este es el método más simple y común para generar números aleatorios en Javascript. Devuelve un número decimal entre 0 y 1. Para obtener un número entero, podemos multiplicar el resultado por un número y luego utilizar el método Math.floor() para redondear hacia abajo.

```
```Javascript
// Ejemplo de generación de un número aleatorio entre 1 y 10
let random = Math.floor(Math.random() * 10) + 1;
console.log(random); // output: un número entre 1 y 10
```

- **Array.length:** Si tenemos un array con varios elementos, podemos utilizar su longitud para generar un número aleatorio que corresponda a uno de los índices del array. Esto es útil cuando queremos mostrar un elemento aleatorio de una lista.

```
```Javascript
// Ejemplo de selección aleatoria de un elemento de un array
let lista = ['manzana', 'pera', 'naranja', 'plátano'];
let random = Math.floor(Math.random() * lista.length);
console.log(lista[random]); // output: un elemento aleatorio de la lista
```

- **Date.now():** Otra forma de generar un número aleatorio es utilizando la marca de tiempo de la fecha actual. Esto nos da un número entero único que puede ser utilizado como ID o para asegurar un valor aleatorio.

```
```Javascript
// Ejemplo de generación de un número aleatorio utilizando la marca de tiempo
let timestamp = Date.now();
console.log(timestamp); // output: un número único que cambia cada milisegundo
```

## Profundizando en la generación de números aleatorios

Los métodos mencionados anteriormente son solo algunas formas básicas de generar números aleatorios en Javascript. Si deseas profundizar en el tema, puedes explorar otras opciones como el método crypto.getRandomValues(), que utiliza una fuente de entropía para generar números aleatorios criptográficamente seguros.

También puedes investigar sobre la distribución de estos números aleatorios y cómo puedes utilizarla para ajustar la probabilidad de ciertos resultados en tus aplicaciones.

## Ver también

- [Documentación oficial de Math.random()](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Math/random)
- [Artículo sobre números aleatorios criptográficamente seguros en Javascript](https://flaviocopes.com/javascript-random-secure/)
- [Ejemplos de distribución de números aleatorios en Javascript](https://www.geeksforgeeks.org/javascript-math-random-distribution-functions/)