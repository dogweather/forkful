---
title:                "Leyendo un archivo de texto"
html_title:           "TypeScript: Leyendo un archivo de texto"
simple_title:         "Leyendo un archivo de texto"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/typescript/reading-a-text-file.md"
---

{{< edit_this_page >}}

## ¿Qué y por qué?
Leer un archivo de texto es una tarea esencial para los programadores. Se refiere a la acción de acceder al contenido de un archivo de texto y utilizarlo en un programa. Los programadores suelen utilizar esta técnica para obtener datos de un archivo o para escribir en él.

## ¿Cómo hacerlo?
En TypeScript, la forma más común de leer un archivo de texto es utilizando la función `readFileSync()` de Node.js. Veamos un ejemplo sencillo de cómo hacerlo:

```TypeScript
import * as fs from 'fs';

const contenido = fs.readFileSync('archivo.txt', 'utf8');
console.log(contenido);
```

Este código importa el módulo `fs` de Node.js, el cual proporciona varias funciones para trabajar con archivos en el sistema. Luego, se utiliza la función `readFileSync()` para leer el archivo de texto especificado como primer argumento. El segundo argumento, en este caso `'utf8'`, indica que se desea obtener el contenido del archivo como una cadena de texto en lugar de un búfer de bytes. Finalmente, se imprime el contenido del archivo en la consola utilizando la función `console.log()`.

### Resultado:

`Este es el contenido del archivo de texto.`

## Profundizando
La lectura de archivos de texto es una técnica ampliamente utilizada en la programación desde hace décadas. Antes de la llegada de Node.js, los lenguajes de programación tenían sus propias formas de leer archivos de texto, como por ejemplo en C se utiliza la función `fopen()`. Sin embargo, el uso de funciones como `readFileSync()` en Node.js hace que esta tarea sea más sencilla y accesible para los programadores.

En TypeScript, también es posible utilizar la función `readFile()` de Node.js, la cual es asincrónica y utiliza callbacks para manejar el resultado de la operación. Además, existen librerías externas como `fs-extra` que proporcionan funciones adicionales para trabajar con archivos y directorios.

## Ver también
- [Documentación de Node.js sobre la función `readFileSync()`](https://nodejs.org/api/fs.html#fs_fs_readfilesync_path_options)
- [Node.js para principiantes: trabajar con archivos](https://www.digitalocean.com/community/tutorials/nodejs-fundamentals-nodejs-file-system)
- [fs-extra: una librería de Node.js para trabajar con archivos y directorios](https://www.npmjs.com/package/fs-extra)