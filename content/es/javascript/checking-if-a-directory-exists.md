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

## ¿Qué & Por qué?
Comprobar si un directorio existe es una tarea común para los programadores en Javascript. Es un proceso que permite a los programadores asegurarse de que una ruta de directorio válida esté disponible antes de acceder a ella o crear nuevos archivos en ella.

## ¿Cómo hacerlo?
Hay varias formas de comprobar si un directorio existe en Javascript. Una forma es utilizar el método ```fs.existsSync()```, que devuelve un valor booleano indicando si la ruta especificada existe. Por ejemplo:

```Javascript
const fs = require('fs');
if (fs.existsSync('ruta/al/directorio')) {
  console.log('El directorio existe');
} else {
  console.log('El directorio no existe');
}
```

Otra forma es utilizar el método ```fs.statSync()```, que devuelve información sobre el archivo o directorio especificado. Si no se encuentra el directorio, se lanzará un error y se puede manejar utilizando un bloque ```try...catch```. Por ejemplo:

```Javascript
const fs = require('fs');
try {
    fs.statSync('ruta/al/directorio');
    console.log('El directorio existe');
} catch (err) {
    console.log('El directorio no existe');
}
```

## Inmersión Profunda
Antes de la versión ES2018, no había una forma nativa de comprobar si un directorio existe en Javascript. Los programadores solían utilizar módulos de terceros como ```fs-extra``` o ```fs-jetpack``` para facilitar esta tarea. Sin embargo, con la adición del módulo ```fs``` en la versión ES2018, ahora es posible hacerlo de forma nativa.

Además de las dos formas mencionadas anteriormente, también se pueden utilizar otras funciones como ```fs.readdirSync()``` y ```fs.accessSync()``` para comprobar si un directorio existe. Sin embargo, es importante tener en cuenta que estas funciones también pueden devolver información sobre archivos en un directorio, no solo sobre el directorio en sí.

En cuanto a la implementación, es importante tener en cuenta que es posible que las rutas de directorios varíen según el sistema operativo y es importante asegurarse de utilizar la ruta correcta dependiendo del sistema operativo en el que se esté trabajando.

## Ver También
- Documentación de Node.js sobre el módulo ```fs```: https://nodejs.org/api/fs.html
- Tutorial sobre cómo comprobar si un directorio existe en Javascript: https://www.geeksforgeeks.org/node-js-fs-existsync-method/