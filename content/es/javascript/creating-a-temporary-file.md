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

## ¿Qué y por qué?
Crear un archivo temporal es una tarea común en la programación, se refiere a la creación de un archivo temporal en la computadora para almacenar datos temporales o trabajar con ellos en un código. Los programadores suelen utilizar archivos temporales para organizar y estructurar mejor su código, así como para realizar pruebas y depuración de forma más eficiente.

## Cómo hacerlo:
``` Javascript
// Para crear una instancia de un archivo temporal en Node.js:
const fs = require('fs');
const tmp = require('tmp'); // Instalar el módulo desde npm first
tmp.file(function _tempFileCreated(err, path, fd, cleanupCallback) {
  if (err) throw err;

  // Escribir datos en el archivo temporal
  fs.write(fd, 'Hello World!', function(err, written, string) {
    // Actualizar los cambios en el archivo
    fs.fdatasync(fd, function(err) {
      // Limpiar y borrar el archivo temporal
      cleanupCallback();
    });
  });
  // ¡El archivo se ha creado con éxito!
  console.log(`Se ha creado el archivo temporal en ${path}`);
});
```

## Detalles en profundo:
- Históricamente, los archivos temporales se utilizaban principalmente en sistemas operativos multitarea para compartir datos entre procesos o programas y así llevar a cabo tareas complejas.
- Aunque usar archivos temporales puede ser útil, también puede ser un indicador de un diseño de código poco eficiente. Es preferible utilizar otras técnicas como variables temporales en la memoria en lugar de almacenar datos temporales en archivos.
- La creación de archivos temporales también se puede realizar utilizando el comando `mktemp` en sistemas UNIX o con la función `tempnam()` en PHP, entre otros métodos.

## Ver también:
- Más información sobre la creación de archivos temporales en Node.js [aquí] (https://nodejs.org/api/fs.html#fs_fs_mkstemp_prefix_options_callback).
- Una guía en profundidad sobre la manipulación de archivos temporales en Java [aquí] (https://www.baeldung.com/java-temporary-files).