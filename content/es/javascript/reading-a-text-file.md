---
title:                "Leyendo un archivo de texto"
html_title:           "Arduino: Leyendo un archivo de texto"
simple_title:         "Leyendo un archivo de texto"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/javascript/reading-a-text-file.md"
---

{{< edit_this_page >}}

# Leer un archivo de texto en Javascript

## ¿Qué y Por qué?
Leer un archivo de texto implica procesar datos almacenados en un archivo de texto (.txt). Los programadores lo hacen para extraer, manipular o analizar datos.

## ¿Cómo hacerlo?
El paquete `fs` (File System) de Node.js permite leer archivos de texto. Aquí hay un código de ejemplo:

```Javascript
const fs = require('fs');

fs.readFile('archivo.txt', 'utf8' , (err, data) => {
  if (err) {
    console.error(err);
    return;
  }
  console.log(data);
});
```

Este código lee `archivo.txt` y muestra su contenido. Si hay un error, se muestra en la consola.

## Análisis profundo
Históricamente, las operaciones de lectura de archivos han estado presentes desde los primeros días de la programación. En Javascript, se explora principalmente con Node.js debido a la falta de capacidad de lectura de archivos en el navegador por razones de seguridad.

Las alternativas a `fs` incluyen `readline` y `stream`. `fs.readFileSync` es una versión síncrona que bloquea la ejecución hasta que se completa la lectura.

Algunos detalles de implementación a tener en cuenta son los problemas de rendimiento con archivos grandes y el manejo de errores durante la lectura.

## Ver también
- Documentación de Node.js: [File System](https://nodejs.org/api/fs.html)
- Uso del paquete [readline](https://nodejs.org/api/readline.html)
- Uso de [streams](https://nodejs.org/api/stream.html) en Node.js