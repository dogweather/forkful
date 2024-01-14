---
title:    "Javascript: Comprobando si existe un directorio"
keywords: ["Javascript"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/es/javascript/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

# Por qué comprobar si un directorio existe en Javascript

A medida que trabajamos en proyectos de Javascript cada vez más complejos, es inevitable que surjan problemas relacionados con la existencia de directorios en nuestro sistema. Por ello, es importante conocer cómo verificar si un directorio existe antes de intentar acceder a él en nuestro código.

## Cómo hacerlo

Para comprobar si un directorio existe en Javascript, podemos utilizar la función `fs.existsSync()` del módulo `fs`. Esta función recibe como argumento la ruta del directorio que queremos verificar y devuelve un valor booleano indicando si el directorio existe o no.

```Javascript
const fs = require('fs');

const path = '/ruta/al/directorio';

if(fs.existsSync(path)) {
  console.log('El directorio existe');
} else {
  console.log('El directorio no existe');
}
```

Si el directorio existe, se imprimirá en la consola "El directorio existe", de lo contrario, se imprimirá "El directorio no existe".

## Profundizando

Es importante tener en cuenta que la función `fs.existsSync()` solo comprueba la existencia del directorio, no si tenemos permisos para acceder a él. Para verificar los permisos, podemos utilizar la función `fs.accessSync()`, que recibe como argumento el directorio y el modo de acceso (lectura, escritura o ejecución).

Otra consideración importante es que la función `fs.existsSync()` solo admite rutas relativas en sistemas Windows. Si necesitamos verificar la existencia de un directorio en un sistema Unix, es necesario utilizar la función `fs.statSync()`, que admite rutas absolutas.

## Ver también

- [Documentación oficial de Node.js sobre la función fs.existsSync()](https://nodejs.org/api/fs.html#fs_fs_existssync_path)
- [Tutorial sobre el uso del módulo fs en Javascript](https://www.w3schools.com/nodejs/nodejs_filesystem.asp)