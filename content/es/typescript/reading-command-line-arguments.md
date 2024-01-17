---
title:                "Leyendo argumentos de línea de comando"
html_title:           "TypeScript: Leyendo argumentos de línea de comando"
simple_title:         "Leyendo argumentos de línea de comando"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/typescript/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## ¿Qué y por qué?
Leer los argumentos de la línea de comandos es una técnica utilizada por los programadores para acceder a la información proporcionada por el usuario a través de la línea de comandos al ejecutar un programa. Esta información puede ser útil para personalizar la ejecución del programa o para enviar mensajes al usuario.

## Cómo:
```TypeScript
// Ejemplo de código para leer argumentos de la línea de comando en TypeScript
let args = process.argv.slice(2); // Eliminar los primeros 2 elementos del array, que son "node" y el nombre del archivo
console.log(args); // Imprime los argumentos proporcionados por el usuario
```

Si ejecutamos este código con el comando `node arguments.ts mensaje1 mensaje2` en nuestra terminal, obtendremos el siguiente resultado:

```TypeScript
["mensaje1", "mensaje2"]
```

## Deep Dive:
Leer los argumentos de la línea de comandos es una técnica muy utilizada en programación, especialmente en aplicaciones y scripts que son ejecutados desde un terminal. Esta técnica permite a los programadores acceder a la información proporcionada por el usuario de forma rápida y sencilla.

Además de utilizar el objeto `process` en TypeScript, también es posible leer los argumentos de la línea de comandos utilizando librerías externas como `yargs` o `commander`, que proporcionan una sintaxis más clara y sencilla para trabajar con argumentos en la línea de comandos.

Para implementar la lectura de argumentos en TypeScript, se utiliza principalmente el método `process.argv.slice()` que nos permite eliminar los primeros elementos del array que contienen información que no es relevante para nuestros argumentos.

## Ver también:
- Documentación oficial de TypeScript para el objeto `process`: https://www.typescriptlang.org/docs/handbook/integrating-with-build-tools.html#using-process
- Librería `yargs` para facilitar el manejo de argumentos en la línea de comandos: https://www.npmjs.com/package/yargs
- Librería `commander` para crear interfaces de línea de comandos de forma intuitiva en TypeScript: https://www.npmjs.com/package/commander