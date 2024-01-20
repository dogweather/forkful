---
title:                "Leyendo argumentos de la línea de comandos"
html_title:           "Bash: Leyendo argumentos de la línea de comandos"
simple_title:         "Leyendo argumentos de la línea de comandos"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/typescript/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## ¿Qué y Por qué?

Leer argumentos de línea de comando es cuando un programa recibe datos después de su nombre cuando se ejecuta desde la línea de comando. Los programadores lo hacen para personalizar el comportamiento de un programa basándose en las entradas proporcionadas por el usuario.

## Cómo hacerlo:

En TypeScript, podemos utilizar el vector `process.argv` de Node.js para acceder a los argumentos de línea de comando.

```TypeScript
#!/usr/bin/env node
let myArg = process.argv[2];  
console.log(`Hola, ${myArg}`);
```
Si ejecutamos este script con `node miScript.js Pedro`, la salida será `Hola, Pedro`.

## Inmersión Profunda:

1. **Contexto Histórico**: Los argumentos de línea de comando han existido desde los días de los sistemas operativos shell. Inicialmente, ofrecían una forma de interactuar con programas en un entorno sin interfaz gráfica de usuario (GUI).
2. **Alternativas**: Además de `process.argv`, hay otros módulos que proporcionan una interfaz más rica para trabajar con argumentos de línea de comando como yargs, minimist, etc.
3. **Detalles de Implementación**: En Node.js y TypeScript, `process.argv` es un arreglo que contiene los argumentos de la línea de comando. El primer elemento será 'node', el segundo elemento será el nombre del archivo JavaScript. Los siguientes elementos serán cualquier argumento de línea de comando adicional.

## Ver También:

1. La [Documentación Oficial de Node.js](https://nodejs.org/docs/latest/api/process.html#process_process_argv) brinda más detalles sobre `process.argv`.
2. El paquete [yargs](https://www.npmjs.com/package/yargs) ofrece una forma más detallada y robusta de manejar argumentos de línea de comando.
3. Para una profundidad histórica, vea el [manual en línea de Unix](http://man7.org/linux/man-pages/man1/bash.1.html) que proporciona una visión general completa de las funciones de línea de comandos.