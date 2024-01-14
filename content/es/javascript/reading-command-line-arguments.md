---
title:    "Javascript: Leyendo argumentos de línea de comando"
keywords: ["Javascript"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/es/javascript/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Por Qué

En el mundo de la programación, es importante poder interactuar con el usuario y recibir información para que nuestros programas sean más dinámicos y personalizados. Una forma de hacerlo es a través de los argumentos de línea de comando, que permiten que los usuarios proporcionen información directamente al ejecutar un programa.

## Cómo Hacerlo

Para leer los argumentos de línea de comando en JavaScript, primero debemos importar el módulo `process` que viene incluido de forma predeterminada en Node.js. Luego, podemos utilizar la función `process.argv` para acceder a una matriz que contiene todos los argumentos proporcionados.

```Javascript
// Importar el módulo process
const process = require('process');

// Acceder a los argumentos
let args = process.argv;

// Imprimir el primer argumento (el segundo elemento en la matriz)
console.log(args[2]);
```

Imaginemos que tenemos un archivo JS llamado `saludo.js` y lo ejecutamos en la línea de comando de la siguiente manera:

```
node saludo.js Hola Mundo
```

El output será "Hola" ya que el primer argumento, `args[2]`, es "Hola".

## Profundizando

Además de acceder a los argumentos individuales, también podemos iterar a través de ellos y realizar acciones específicas. Por ejemplo, podemos crear una función que sume todos los números pasados como argumentos:

```Javascript
// Importar el módulo process
const process = require('process');

// Acceder a los argumentos
let args = process.argv;

// Definir una función para sumar los números
function sumarNumeros() {
  let suma = 0;
  // Empezamos en el tercer elemento (args[2]) ya que los dos primeros son "node" y el nombre del archivo
  for (let i = 2; i < args.length; i++) {
    suma += parseInt(args[i]); // Utilizamos parseInt porque los argumentos son recibidos como strings
  }
  return suma;
}

// Imprimir la suma de los argumentos numéricos
console.log(sumarNumeros());
```

Si ejecutamos el mismo archivo `saludo.js` con números como argumentos:

```
node saludo.js 5 10 15
```

El output será "30" ya que se sumaron todos los argumentos numéricos.

## Ver También

- [Documentación de Node.js sobre `process.argv`](https://nodejs.org/docs/latest/api/process.html#process_process_argv) 
- [Tutorial de Codecademy sobre argumentos de línea de comando en JavaScript](https://www.codecademy.com/learn/introduction-to-javascript/modules/learn-javascript-syntax/cheatsheet) (en inglés)
- [Vídeo tutorial de FreeCodeCamp sobre argumentos de línea de comando en Node.js](https://www.freecodecamp.org/news/command-line-arguments-nodejs/) (en inglés)