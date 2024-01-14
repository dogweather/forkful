---
title:                "Javascript: Escribiendo un archivo de texto"
simple_title:         "Escribiendo un archivo de texto"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/javascript/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Por qué

Escribir un archivo de texto es una habilidad fundamental para cualquier programador de Javascript. Esto nos permite almacenar y organizar información en un formato legible para nosotros y para la computadora. Ya sea para crear una lista de tareas, un archivo de configuración o incluso un archivo de código, escribir un texto es una habilidad esencial para cualquier proyecto.

## Cómo hacerlo

Para escribir un archivo de texto en Javascript, utilizamos la función `writeFile` del módulo `fs`. Primero, debemos importar este módulo en nuestro código usando la palabra clave `require`:

```Javascript
const fs = require('fs');
```

Luego, utilizamos la función `writeFile` de la siguiente manera:

```Javascript
fs.writeFile('mi-archivo.txt', 'Este es el contenido de mi archivo', (err) => {
  if (err) throw err;
  console.log('¡Archivo creado exitosamente!');
});
```

La primera parte del código es la ruta y el nombre del archivo que queremos crear, en este caso `mi-archivo.txt`. Luego, pasamos el contenido que queremos escribir en el archivo, en este caso `'Este es el contenido de mi archivo'`. Finalmente, la función toma un tercer argumento, que es una función de retorno de llamada que se ejecutará una vez que el archivo se haya creado con éxito.

Si queremos añadir más contenido a nuestro archivo en lugar de reemplazarlo, podemos usar la función `appendFile` en su lugar.

```Javascript
fs.appendFile('mi-archivo.txt', 'Este es un texto adicional', (err) => {
  if (err) throw err;
  console.log('¡Contenido agregado exitosamente!');
});
```

## Profundizando

Además de escribir y agregar contenido a archivos de texto, también podemos leer y eliminar archivos utilizando el módulo `fs` en Javascript. Esto nos permite tener un control completo sobre la manipulación de archivos en nuestro código.

Para leer un archivo de texto, utilizamos la función `readFile`. Al igual que con `writeFile`, pasamos la ruta del archivo y una función de retorno de llamada. Sin embargo, esta vez el contenido del archivo se pasa como un argumento a esta función de retorno de llamada.

```Javascript
fs.readFile('mi-archivo.txt', (err, data) => {
  if (err) throw err;
  console.log(data); // Imprime el contenido del archivo
});
```

Para eliminar un archivo, utilizamos la función `unlink` y proporcionamos la ruta del archivo que queremos borrar. Esta función también toma una función de retorno de llamada que se ejecutará una vez que el archivo se haya eliminado correctamente.

```Javascript
fs.unlink('mi-archivo.txt', (err) => {
  if (err) throw err;
  console.log('¡Archivo eliminado exitosamente!');
});
```

Con estas funciones, podemos escribir, leer y eliminar archivos de texto en Javascript de manera sencilla y eficiente.

## Ver también

- [Documentación de Node.js: Módulo fs](https://nodejs.org/api/fs.html)
- [Tutorial de Node.js: Manipulación de archivos con el módulo fs](https://www.w3schools.com/nodejs/nodejs_filesystem.asp)
- [Escribir y leer archivos con Javascript](https://www.freecodecamp.org/news/node-js-tutorial-write-files-in-node-js-using-the-fs-module/)