---
title:                "Leyendo un archivo de texto"
html_title:           "TypeScript: Leyendo un archivo de texto"
simple_title:         "Leyendo un archivo de texto"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/typescript/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Por qué

Si estás buscando un lenguaje de programación que sea fácil de aprender, flexible y basado en JavaScript, entonces TypeScript podría ser lo que necesitas. La lectura de archivos de texto es una tarea común en el desarrollo de aplicaciones, y en este artículo te explicaremos cómo hacerlo utilizando TypeScript.

## Cómo hacerlo

```TypeScript
//Importar el módulo 'fs' que nos permite acceder al sistema de archivos
import * as fs from 'fs';

//Leer el contenido completo de un archivo de texto
fs.readFile('miArchivo.txt', 'utf8', (err, data) => {
  if (err) throw err;
  console.log(data);
})

//Leer línea por línea y mostrar el resultado en la consola
const stream = fs.createReadStream('miArchivo.txt', { encoding: 'utf8' });
let tempString = '';

stream.on('data', (chunk) => {
  //Convirtiendo los datos a una cadena de texto
  tempString += chunk.toString();
  //Separamos la cadena en líneas
  const lines = tempString.split(/\r\n|\r|\n/);
  //Mostramos el contenido línea por línea
  lines.forEach((line) => {
    console.log(line);
  });
})
```

## Profundizando

La función `readFile()` de Node.js es la forma más sencilla de leer un archivo de texto en TypeScript. Utiliza un callback para recibir el contenido del archivo y manejar los errores en caso de que existan. Por otro lado, la función `createReadStream()` nos permite leer el archivo línea por línea, lo que puede ser útil para archivos grandes o para realizar operaciones más avanzadas.

## Ver también

- [Documentación de Node.js sobre el módulo 'fs'](https://nodejs.org/api/fs.html)
- [Guía de TypeScript para principiantes](https://www.typescriptlang.org/docs/handbook/typescript-in-5-minutes.html)
- [Ejemplos de lectura de archivos de texto en TypeScript](https://github.com/Ruthenic/file-reading-examples)