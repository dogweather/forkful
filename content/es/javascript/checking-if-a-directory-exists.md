---
title:                "Comprobando si existe un directorio"
html_title:           "Javascript: Comprobando si existe un directorio"
simple_title:         "Comprobando si existe un directorio"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/javascript/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Por qué 

Si eres un desarrollador de Javascript, es probable que en algún momento necesites verificar si un directorio existe dentro de tu código. Esto te ayudará a asegurarte de que tu programa se esté ejecutando correctamente y a tomar decisiones basadas en la existencia o no de una ruta específica.

## Cómo hacerlo 

Para verificar si un directorio existe, podemos utilizar la función `fs.existsSync()` de Node.js. Esta función nos permite verificar si una ruta existe o no en el sistema de archivos. Veamos un ejemplo de cómo utilizarla:

```javascript
const fs = require('fs');

if (fs.existsSync('/mi/directorio')) {
  console.log('El directorio existe');
} else {
  console.log('El directorio no existe');
}
```

Este código primero importa el módulo `fs` de Node.js y luego utiliza la función `existsSync()` para verificar si la ruta `/mi/directorio` existe. Si la ruta existe, se imprimirá en la consola "El directorio existe", de lo contrario, se imprimirá "El directorio no existe".

## Deep Dive 

Cuando utilizamos la función `fs.existsSync()`, en realidad estamos verificando si existe un archivo o un directorio en la ruta especificada. Si queremos asegurarnos de que el elemento en la ruta sea un directorio, podemos utilizar la función `fs.statSync()` y verificar la propiedad `isDirectory()` del objeto devuelto. Por ejemplo:

```javascript
const fs = require('fs');

if (fs.existsSync('/mi/directorio')) {
  const stats = fs.statSync('/mi/directorio');
  if (stats.isDirectory()) {
    console.log('La ruta especificada es un directorio');
  } else {
    console.log('La ruta especificada no es un directorio');
  }
} else {
  console.log('El directorio no existe');
}
```

Al utilizar la función `fs.statSync()`, obtenemos un objeto con información sobre el elemento en la ruta especificada. Luego podemos utilizar la propiedad `isDirectory()` para determinar si el elemento es un directorio o no.

## Ver también 

- Documentación de Node.js sobre la función `fs.existsSync()`: https://nodejs.org/api/fs.html#fs_fs_existssync_path
- Documentación de Node.js sobre la función `fs.statSync()`: https://nodejs.org/api/fs.html#fs_fs_statsync_path_options