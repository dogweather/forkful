---
title:                "Javascript: Generando números aleatorios"
programming_language: "Javascript"
category:             "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/javascript/generating-random-numbers.md"
---

{{< edit_this_page >}}

# Por qué utilizar números aleatorios en programación

En programación, la generación de números aleatorios es una herramienta muy útil para simular situaciones impredecibles o crear juegos y aplicaciones interactivas. Al utilizar números aleatorios, podemos proporcionar una experiencia diferente cada vez que se ejecuta el código, lo que hace que la aplicación sea más interesante y atractiva para el usuario.

# Cómo generar números aleatorios en Javascript

En Javascript, podemos generar números aleatorios utilizando la función `Math.random()` que nos devuelve un número decimal entre 0 y 1. Podemos multiplicar este número por el rango que deseamos y luego redondearlo utilizando la función `Math.floor()` para obtener un número entero.

```Javascript
let randomNumber = Math.floor(Math.random() * 10) + 1; // genera un número aleatorio entre 1 y 10
console.log(randomNumber); // imprimirá un número aleatorio cada vez que se ejecute el código
```

También podemos utilizar la función `Math.random()` para seleccionar un elemento aleatorio de un array:

```Javascript
let fruits = ['manzana', 'banana', 'naranja', 'pera', 'uva'];
let randomFruit = fruits[Math.floor(Math.random() * fruits.length)]; // seleccionará un elemento aleatorio del array "fruits"
console.log(randomFruit); // imprimirá un elemento aleatorio del array cada vez que se ejecute el código
```

# Un vistazo más profundo a la generación de números aleatorios

Aunque la función `Math.random()` es suficiente para la mayoría de los casos, algunos desarrolladores pueden necesitar un mayor control sobre los números aleatorios que se generan. En estos casos, podemos utilizar bibliotecas externas como "random.js" que nos permiten establecer una semilla para los números aleatorios o generar números aleatorios en un rango específico.

Además, hay que tener en cuenta que la función `Math.random()` se basa en el reloj interno de la computadora, por lo que en teoría, si ejecutamos el mismo código en el mismo momento, obtendremos el mismo número aleatorio. Para evitar esto, podemos utilizar el método `Date.now()` para generar la semilla inicial en lugar del reloj de la computadora.

# Ver también

- [Math.random() en MDN](https://developer.mozilla.org/es/docs/Web/JavaScript/Reference/Global_Objects/Math/random)
- [random.js en GitHub](https://github.com/ckknight/random-js)
- [Generar números aleatorios en JS con semilla en Stack Overflow](https://stackoverflow.com/questions/521295/seeding-the-random-number-generator-in-javascript)