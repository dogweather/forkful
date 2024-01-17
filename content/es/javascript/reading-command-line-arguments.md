---
title:                "Leyendo argumentos de línea de comandos"
html_title:           "Javascript: Leyendo argumentos de línea de comandos"
simple_title:         "Leyendo argumentos de línea de comandos"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/javascript/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## ¿Qué & Por qué?
Leer argumentos de línea de comando es una forma en que los programadores pueden interactuar con su código mediante la introducción de información directamente desde la línea de comando de su terminal. Esto puede ser útil para proporcionar opciones o parámetros personalizados a un programa, permitiendo así una mayor flexibilidad en su uso.

## Cómo:
A continuación se presentan dos ejemplos de cómo leer argumentos de línea de comando en Javascript, junto con la salida resultante:

```Javascript
// Ejemplo 1:
process.argv.forEach((val, index) => {
  console.log(`${index}: ${val}`);
});

/* Salida:
0: node
1: index.js
2: argumento1
3: argumento2
*/
```

```Javascript
// Ejemplo 2:
console.log(process.argv[2]);

/* Salida:
argumento1
*/
```

## Deep Dive:
La lectura de argumentos de línea de comando en la programación ha existido desde los inicios del desarrollo de software, y sigue siendo una opción popular para interactuar con programas. Sin embargo, existen alternativas, como la creación de una interfaz de usuario o el uso de variables de entorno. La implementación de cómo leer argumentos de línea de comando puede variar según el lenguaje de programación que se esté utilizando, y en Javascript se puede utilizar el objeto `process` para acceder a ellos.

## See Also:
- [Documentación de `process.argv`](https://nodejs.org/docs/latest/api/process.html#process_process_argv)
- [Cómo leer argumentos de línea de comando en Node.js](https://www.digitalocean.com/community/tutorials/nodejs-command-line-arguments-es)