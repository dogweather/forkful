---
title:                "Javascript: Comprobando si existe un directorio"
programming_language: "Javascript"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/javascript/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Por qué

Al trabajar con Javascript, es importante asegurarse de que los directorios existan antes de intentar acceder a ellos en nuestro código. Esto nos ayuda a evitar errores y asegurar un funcionamiento correcto de nuestras aplicaciones.

## Cómo hacerlo

Para comprobar si un directorio existe en Javascript, podemos usar la función `fs.existsSync()` del módulo `fs`. Esta función devuelve un valor booleano, `true` si el directorio existe y `false` en caso contrario. A continuación, se muestra un ejemplo de cómo utilizar esta función:

```Javascript
const fs = require('fs');

// Comprobamos si existe el directorio "miDirectorio"
if (fs.existsSync("miDirectorio")) {
    console.log("El directorio existe");
} else {
    console.log("El directorio no existe");
}
```

En este ejemplo, usamos la sentencia `if` para evaluar el valor devuelto por `fs.existsSync()` y mostrar un mensaje según corresponda. También podemos almacenar el valor devuelto en una variable y utilizarla más adelante en nuestro código.

## Detalles técnicos

Para aquellos interesados en conocer cómo funciona el método `fs.existsSync()`, podemos profundizar un poco más en su implementación. Esta función realiza una llamada al sistema para comprobar si el directorio existe en el sistema de archivos. En caso de que exista, devuelve `true`, de lo contrario, devuelve `false`. Es importante destacar que esta función solo comprueba si el directorio existe, no si tenemos permisos para acceder a él.

## Ver también

- Documentación oficial de `fs.existsSync()`: https://node.readthedocs.io/en/latest/api/fs/#fsnameexistsync
- Ejemplos prácticos de uso de `fs.existsSync()`: https://www.geeksforgeeks.org/node-js-fs-existsync-method/
- Tutorial en video sobre cómo comprobar la existencia de un directorio en Javascript: https://www.youtube.com/watch?v=jPui8ljm6Mw