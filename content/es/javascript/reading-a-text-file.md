---
title:                "Leyendo un archivo de texto"
html_title:           "Javascript: Leyendo un archivo de texto"
simple_title:         "Leyendo un archivo de texto"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/javascript/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Por qué
La lectura de archivos de texto es una habilidad esencial en el desarrollo de Javascript. Ahora, más que nunca, la capacidad de leer y manipular datos de texto es crucial en la creación de aplicaciones web dinámicas y sofisticadas.

## Cómo hacerlo
```Javascript
let fs = require('fs'); // importar el módulo file system

// leer el archivo de texto de manera asíncrona
fs.readFile('datos.txt', 'utf8', (err, data) => { 
    if (err) throw err; // manejar errores
    console.log(data); // imprimir los datos del archivo
});

// leer el archivo de texto de manera síncrona
let data = fs.readFileSync('datos.txt', 'utf8'); 
console.log(data); // imprimir los datos del archivo
```

Si se desea leer un archivo que no está en formato UTF-8, se puede especificar la codificación adecuada en el segundo parámetro de la función `readFile()` o en la función `readFileSync()`.

## Profundizando
Además de leer el contenido de un archivo de texto, es importante también conocer y manejar la estructura del mismo. Una vez que se ha leído el archivo, se puede dividir el contenido en líneas o en palabras utilizando funciones como `split()`, `match()`, `slice()` y `substr()`.

También es posible manipular el contenido del archivo antes de imprimirlo o guardarlo en una variable, e incluso escribir en él utilizando la función `writeFile()` o `appendFile()`.

## Ver también
- [Documentación oficial de Node.js sobre el módulo file system](https://nodejs.org/api/fs.html)
- [Tutorial de codeburst sobre la lectura de archivos de texto en Javascript](https://codeburst.io/reading-a-file-with-node-js-9d5e8076686d)
- [Artículo de Medium sobre la manipulación de archivos de texto en Javascript](https://medium.com/@anirudhgiri/reading-and-writing-text-files-in-javascript-local-file-system-462ffacc9702)