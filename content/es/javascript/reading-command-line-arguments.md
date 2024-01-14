---
title:                "Javascript: Leyendo argumentos de línea de comando"
simple_title:         "Leyendo argumentos de línea de comando"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/javascript/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Por Qué


Si eres un programador en JavaScript, es posible que te hayas encontrado con la necesidad de leer los argumentos de línea de comandos en tus programas. Esto puede ser útil para personalizar la ejecución de tu código o para interactuar con el usuario de manera más dinámica. En esta publicación, exploraremos por qué es importante saber cómo leer los argumentos de línea de comandos en JavaScript.

## Cómo Hacerlo

Leer los argumentos de línea de comandos en JavaScript es un proceso sencillo y te permitirá ampliar las funcionalidades de tus programas. Para hacerlo, puedes utilizar el objeto "process" que viene integrado en la plataforma de Node.js. Aquí hay un ejemplo de código que muestra cómo leer un argumento de línea de comandos y mostrarlo por consola:

```Javascript
// Creamos una variable para almacenar el primer argumento de línea de comandos
let argumento = process.argv[2];

// Mostramos el argumento por consola
console.log(argumento);
```

Si ejecutamos este código pasando un argumento después del nombre del archivo, por ejemplo "node programa.js Hola", el resultado sería "Hola". Si no pasamos ningún argumento, simplemente se mostrará "undefined". Puedes jugar con este código y probar diferentes escenarios para familiarizarte con el concepto.

## Inmersión Profunda

Ahora que ya sabes cómo leer los argumentos de línea de comandos en JavaScript, veamos algunos detalles adicionales que pueden ser útiles. Como mencionamos anteriormente, el objeto "process" contiene todos los argumentos de línea de comandos en su propiedad "argv". Esto regresa un array donde el primer elemento es la ruta del ejecutable de Node, el segundo elemento es la ruta del archivo que se está ejecutando y los siguientes elementos son los argumentos de línea de comandos que se hayan pasado. Por ejemplo:

```Javascript
// Imprimimos todos los argumentos por consola
console.log(process.argv);

// Resultado si ejecutamos "node programa.js Hola Mundo"
// [
//   'C:\\Program Files\\nodejs\\node.exe',
//   'C:\\ruta\\a\\nuestro\\programa.js',
//   'Hola',
//   'Mundo'
// ]
```

Esto puede ser útil si quieres leer más de un argumento o si no estás seguro de cuántos argumentos pueden pasarle a tu programa. También puedes usar diferentes métodos de array, como "slice" o "filter", para obtener solo los argumentos que necesitas.

## Ver También

- [Documentación de Node.js sobre el objeto "process"](https://nodejs.org/dist/latest-v16.x/docs/api/process.html)
- [Video tutorial sobre cómo leer argumentos de línea de comandos en JavaScript](https://www.youtube.com/watch?v=pYOltVz7kL0)
- [Ejemplos de código en GitHub sobre cómo manejar argumentos de línea de comandos](https