---
title:                "Creando un archivo temporal"
html_title:           "TypeScript: Creando un archivo temporal"
simple_title:         "Creando un archivo temporal"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/typescript/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Por qué

Crear archivos temporales es una práctica muy útil en programación para almacenar datos de forma temporal y no ocupar espacio en disco innecesariamente. Además, pueden ser utilizados para realizar pruebas y experimentar con diferentes datos sin afectar al funcionamiento del código original.

## Cómo hacerlo

Para crear un archivo temporal en TypeScript, podemos utilizar la función `fs.mkdtempSync()` proporcionada por el módulo `fs` de Node.js. Esta función acepta dos parámetros: un prefijo para el nombre del archivo y una función de retorno (callback) que nos devuelve el nombre y la ruta del archivo temporal creado. Veamos un ejemplo:

```TypeScript
const fs = require('fs');
const path = require('path');

const prefix = 'tempFile_';
const tempFile = fs.mkdtempSync(prefix, (err, folder) => {
  if (err) throw err;
  return folder;
});

console.log(tempFile); // output: "tempFile_064faa90-38f9-40b3-b872-9a65f31720d8" (nombre y ruta generados automáticamente)
```

Como se puede ver en el código, al ejecutar la función `mkdtempSync()`, se genera automáticamente un nombre para el archivo temporal utilizando el prefijo que le pasamos como parámetro. Además, se le asigna una ruta aleatoria para evitar posibles conflictos de nombres con otros archivos en nuestra aplicación.

Por otro lado, también podemos utilizar la librería `tmp` para crear archivos temporales de una manera más sencilla y organizada. Esta librería nos permite especificar el prefijo, sufijo, directorio y nombre del archivo temporal que queremos crear. Veamos un ejemplo:

```TypeScript
const tmp = require('tmp');

const tempObj = tmp.fileSync({ prefix: 'tempFile_', postfix: '.txt', dir: '/tmp' });
console.log(tempObj.name); // output: "/tmp/tempFile_302e5446-f057-6812-a173-04c5f77eca15.txt"
```

En este caso, la función `fileSync()` nos devuelve un objeto con la información del archivo temporal que hemos creado, incluyendo su nombre y ruta. Como se puede observar, también podemos especificar un sufijo y directorio para el archivo.

## Profundizando

Crear un archivo temporal puede ser útil en diferentes situaciones, por ejemplo, al trabajar con datos sensibles que no queremos almacenar permanentemente en nuestro sistema o al procesar grandes cantidades de información que no queremos guardar en disco.

Es importante tener en cuenta que, al finalizar nuestra aplicación, debemos borrar los archivos temporales que hemos creado para no ocupar espacio de forma innecesaria en nuestro sistema. Para ello, podemos utilizar la función `fs.unlinkSync()` para borrar el archivo, o la función `fs.rmdirSync()` si queremos borrar todo el directorio en el que se encuentra el archivo temporal.

## Ver también

- Documentación oficial de Node.js sobre `fs.mkdtempSync()`: https://nodejs.org/api/fs.html#fs_fs_mkdtempsync_prefix_options_callback
- Documentación oficial de la librería `tmp`: https://github.com/raszi/node-tmp
- Artículo sobre la gestión de archivos temporales en TypeScript: https://ilikekillnerds.com/2017/02/managing-temporary-files-node-js-typescript/