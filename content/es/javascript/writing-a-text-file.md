---
title:    "Javascript: Escritura de un archivo de texto"
keywords: ["Javascript"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/es/javascript/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Por qué escribir un archivo de texto en Javascript

Escribir un archivo de texto en Javascript puede ser una tarea útil para aquellos que desean almacenar y organizar datos en una forma legible y accesible. Desde guardar pequeñas notas hasta crear un archivo de configuración para una aplicación, escribir un archivo de texto puede ser una herramienta valiosa en el desarrollo de software.

## Cómo hacerlo

Para escribir un archivo de texto en Javascript, se debe utilizar el objeto `fs` de Node.js, que proporciona métodos para interactuar con el sistema de archivos. Primero, se debe requerir el módulo `fs` y crear una instancia del objeto `fs` usando la palabra clave `require`.

```Javascript
// Requerir el modulo `fs`
const fs = require('fs');

// Crear una instancia del objeto `fs`
const archivo = new fs();
```

Luego, se pueden utilizar los métodos proporcionados por el objeto `fs` para escribir en el archivo de texto. Por ejemplo, para crear un nuevo archivo y escribir una línea de texto en él, se puede utilizar el método `writeFile`.

```Javascript
// Escribir una línea de texto en el archivo
fs.writeFile('mi-archivo.txt', 'Este es un ejemplo de texto escrito en un archivo de texto.', (err) => {
  if (err) throw err;
  console.log('¡El archivo ha sido creado y la línea de texto ha sido escrita con éxito!');
});
```

También se puede utilizar el método `appendFile` para agregar texto a un archivo existente en vez de crear uno nuevo.

```Javascript
// Agregar texto al archivo
fs.appendFile('mi-archivo.txt', 'Este es otro ejemplo de texto, que se añadirá a continuación de la primera línea.', (err) => {
  if (err) throw err;
  console.log('¡Se ha agregado el texto con éxito!');
});
```

## Profundizando

Además de escribir líneas de texto en un archivo, también se puede utilizar el objeto `fs` para leer, renombrar, mover o eliminar archivos de texto. Se recomienda revisar la documentación oficial de Node.js para obtener más información sobre cómo trabajar con archivos utilizando el objeto `fs`.

## Ver también

- [Documentación oficial de Node.js sobre el objeto `fs`](https://nodejs.org/api/fs.html)
- [Tutorial sobre cómo escribir y leer archivos de texto en Node.js](https://www.digitalocean.com/community/tutorials/how-to-read-and-write-files-in-node-js)
- [Artículo sobre cómo trabajar con el sistema de archivos en Node.js](https://www.freecodecamp.org/news/node-js-file-system-tutorial-f969e7cb22ba/)