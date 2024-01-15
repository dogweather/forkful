---
title:                "Leyendo argumentos de línea de comandos"
html_title:           "TypeScript: Leyendo argumentos de línea de comandos"
simple_title:         "Leyendo argumentos de línea de comandos"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/typescript/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## ¿Por qué leer argumentos de línea de comandos?

Leer argumentos de línea de comandos es una habilidad esencial en el desarrollo de aplicaciones en TypeScript. Al utilizar argumentos de línea de comandos, podemos hacer que nuestras aplicaciones sean más interactivas y dinámicas, ya que nos permiten ingresar datos personalizados al ejecutar el programa. Además, nos ayuda a tener un mayor control sobre nuestros programas al brindarnos la posibilidad de especificar diferentes opciones de ejecución.

## Cómo leer argumentos de línea de comandos en TypeScript

En TypeScript, podemos acceder a los argumentos de línea de comandos a través de la variable global "process". Para acceder a ella, podemos utilizar la sintaxis "process.argv". Veamos un ejemplo sencillo:

```TypeScript
  // Declaramos una variable para almacenar los argumentos
  let args = process.argv;
  
  // Imprimimos los argumentos en consola
  console.log(args);
```

Si ejecutamos este programa con el comando `node ejemplo.ts argumento1 argumento2`, obtendremos la siguiente salida:

```
[ 'node', 'ejemplo.ts', 'argumento1', 'argumento2' ]
```

Como podemos ver, la variable `args` contiene una lista donde el primer elemento es el comando utilizado para ejecutar el programa y los siguientes son los argumentos ingresados. Podemos utilizar estos argumentos en nuestro programa para realizar diferentes acciones, como por ejemplo:

```TypeScript
// Imprimimos el primer argumento
console.log(`El primer argumento es: ${args[2]}`);
// Imprimimos el segundo argumento
console.log(`El segundo argumento es: ${args[3]}`);
```

La salida de este programa sería:

```
El primer argumento es: argumento1
El segundo argumento es: argumento2
```

## Profundizando en la lectura de argumentos de línea de comandos

La variable `process.argv` nos permite acceder a todos los argumentos ingresados en la línea de comandos, pero a veces también necesitamos especificar opciones de ejecución para nuestro programa. Para esto, podemos utilizar la librería "yargs". Esta librería nos permite crear opciones de ejecución en nuestros programas y acceder a ellas de manera fácil y sencilla. Veamos un ejemplo de cómo utilizarla:

Primero, debemos instalar la librería en nuestro proyecto utilizando el comando:

```
npm install yargs
```

Luego, en nuestro código, podemos utilizarla de la siguiente manera:

```TypeScript
// Importamos la librería
import yargs from 'yargs';

// Creamos una opción de ejecución llamada "saludo"
const argv = yargs.option('saludo', {
  alias: 's',
  description: 'Mensaje de saludo al usuario',
  type: 'string',
  demandOption: true
}).argv;

// Imprimimos el mensaje de saludo ingresado por el usuario
console.log(`¡${argv.saludo}, usuario!`);
```

Si ejecutamos este programa con el comando `node ejemplo.ts --saludo Hola`, obtendremos la siguiente salida:

```
¡Hola, usuario!
```

Como podemos ver, la librería "yargs" nos permite crear opciones más claras y específicas para nuestros programas y acceder a ellas de manera más organizada.

## Ver también

- Documentación oficial de TypeScript: https://www.typescriptlang.org/docs/
- Documentación de la librería "yargs": https://www.npmjs.com/package/yargs