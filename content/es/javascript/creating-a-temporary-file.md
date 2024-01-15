---
title:                "Creando un archivo temporal"
html_title:           "Javascript: Creando un archivo temporal"
simple_title:         "Creando un archivo temporal"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/javascript/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## ¿Por qué crear un archivo temporal en Javascript?

Hay muchas razones por las que un programador podría querer crear un archivo temporal en su aplicación. Es especialmente útil cuando se trabaja con datos sensibles, como contraseñas o información de pago, ya que permite almacenar temporalmente estos datos para su uso y luego eliminarlos de manera segura.

## Cómo crear un archivo temporal en Javascript

Para crear un archivo temporal en Javascript, se puede utilizar la función `createTempFile()` del módulo `fs`. En este ejemplo, crearemos un archivo temporal con el nombre "temp.txt":

```Javascript
const fs = require('fs');
fs.createTempFile('temp.txt', (err, filePath) => {
  if (err) {
    console.error(err);
  } else {
    console.log('Archivo temporal creado en: ' + filePath);
  }
});
```

Este código generará un archivo temporal en la carpeta especificada por el sistema operativo, y devolverá la ruta del archivo creado en caso de éxito.

## Profundizando en la creación de archivos temporales en Javascript

Además de la función `createTempFile()`, el módulo `fs` también ofrece la función `mkdtemp()` para crear una carpeta temporal en lugar de solo un archivo. También se puede especificar una carpeta específica para crear el archivo temporal utilizando la opción `dir` en ambas funciones.

Además, se puede utilizar la opción `keep` para mantener el archivo temporal después de que se cierra el proceso, lo cual puede ser útil para propósitos de depuración.

## Ver también

- [Documentación de Node.js sobre fs.createTempFile()](https://nodejs.org/docs/latest-v10.x/api/fs.html#fs_fs_createtempfile_prefix_options_callback)
- [Ejemplo práctico de creación de un archivo temporal en Node.js](https://www.digitalocean.com/community/tutorials/how-to-use-the-tmp-directory-in-node-js)