---
title:                "Comprobando si existe un directorio"
date:                  2024-01-20T14:57:24.196688-07:00
html_title:           "Gleam: Comprobando si existe un directorio"
simple_title:         "Comprobando si existe un directorio"

category:             "Javascript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/javascript/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## ¿Qué y Por Qué?
Comprobar si un directorio existe es validar la presencia de una carpeta en el sistema de archivos. Los programadores lo hacen para evitar errores al intentar acceder, leer o escribir en un directorio que no está allí.

## Cómo hacerlo:
En Node.js, usamos el módulo `fs` para interactuar con el sistema de archivos. Aquí tienes un ejemplo de cómo verificar si un directorio existe.

```Javascript
const fs = require('fs');
const path = './miDirectorio';

try {
  if (fs.existsSync(path)) {
    console.log('¡El directorio existe!');
  } else {
    console.log('El directorio no existe.');
  }
} catch (err) {
  console.error(err);
}
```
Salida dependiendo de la situación:
```
¡El directorio existe!
```
o
```
El directorio no existe.
```

## Inmersión Profunda:
Antes de Node.js v10, se usaba el método `fs.exists`, que ahora está desaconsejado porque usaba un estilo de callback, que podía llevar a algunas complicaciones y errores de manejo. `fs.existsSync` es síncrono, lo que significa que bloqueará la ejecución de más código hasta que complete su trabajo. Para operaciones asíncronas no bloqueantes, puedes usar `fs.promises.stat` junto con un bloque `try-catch` para manejar excepciones.

Alternativas:
- `fs.stat` o `fs.statSync` para obtener información del directorio y capturar el error si no existe.
- Con promesas: 
```Javascript
const fsPromises = require('fs').promises;

async function checkDirectory(directory) {
  try {
    await fsPromises.access(directory, fs.constants.F_OK);
    console.log('¡El directorio existe!');
  } catch {
    console.log('El directorio no existe.');
  }
}

checkDirectory('./miDirectorio');
```

## Ver También:
- Documentación oficial de Node.js para el módulo 'fs': https://nodejs.org/api/fs.html
- Una guía sobre promesas en JavaScript: https://developer.mozilla.org/es/docs/Web/JavaScript/Guide/Using_promises
- Artículo sobre manejo de errores en asincronía con async/await: https://developer.mozilla.org/es/docs/Learn/JavaScript/Asynchronous/Async_await
