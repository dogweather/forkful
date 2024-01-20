---
title:                "Verificando si un directorio existe"
html_title:           "Javascript: Verificando si un directorio existe"
simple_title:         "Verificando si un directorio existe"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/javascript/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## ¿Qué y Por qué?
Verificar si existe un directorio consiste en confirmar la presencia de un directorio antes de interactuar con este. Los programadores lo hacen para evitar errores cuando se intenta actuar sobre un directorio que no existe.

## ¿Cómo hacerlo?
Javascript no tiene una manera incorporada para verificar si un directorio existe, pero podemos hacerlo utilizando el módulo `fs` de Node.js. Aquí está el código:

```Javascript
const fs = require('fs');

let directorio = './ruta/directorio';
if (fs.existsSync(directorio)){
    console.log("El directorio existe");
} else {
    console.log("El directorio no existe");
}
```
Correr este código devolverá `El directorio existe` si el directorio existe y `El directorio no existe` si no es el caso.

## Análisis Profundo
Históricamente, JavaScript no se diseñó para interactuar con el sistema de archivos, lo que explica la falta de soporte incorporado para estas comprobaciones. Node.js ha llenado este vacío con su integración de `fs`.

Hay alternativas para verificar la existencia de un directorio, como el método `fs.stat()`, que nos proporciona información más detallada sobre el directorio pero puede ser excesivo para solo verificar la existencia de este.

Es importante saber que estos métodos solo comprueban la existencia de un directorio. No garantizan que el programa tenga permisos para leer o escribir en el directorio.

## Ver También
- Documentación de Node.js fs: [https://nodejs.org/api/fs.html](https://nodejs.org/api/fs.html)
- Artículo sobre manejo de archivos y directorios en Node.js: [https://www.w3schools.com/nodejs/nodejs_filesystem.asp](https://www.w3schools.com/nodejs/nodejs_filesystem.asp)