---
title:                "Leyendo un archivo de texto"
html_title:           "Arduino: Leyendo un archivo de texto"
simple_title:         "Leyendo un archivo de texto"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/typescript/reading-a-text-file.md"
---

{{< edit_this_page >}}

## ¿Qué & Por qué?

Leer un archivo de texto significa extraer la información almacenada en esas líneas de texto mediante un programa. Los programadores hacen esto para manipular datos, analizar la información, y automatizar tareas en un archivo de texto.

## Cómo hacerlo:

Para leer un archivo de texto en TypeScript, se usa el paquete built-in `fs` (File System) de Node.js. Asegúrate de tener Node.js en tu proyecto. 

```TypeScript 
const fs = require('fs');

fs.readFile('nombre-del-archivo.txt', 'utf8' , (err, data) => {
  if (err) {
    console.error(err);
  }
  console.log(data);
});
```

En el caso de TypeScript, una alternativa a fs.readFile sería usar fs.promises.readFile:

```TypeScript 
const fs = require('fs').promises;

async function main() {
    const data = await fs.readFile('nombre-del-archivo.txt', 'utf8');
    console.log(data);
}

main().catch(console.error);
```

## Inmersión Profunda:

Leer archivos de texto es una habilidad fundamental que se realiza desde los inicios de la informática. Muchos archivos de configuración y muchas aplicaciones aún dependen de los archivos de texto por su simplicidad.

Existen alternativas a `fs` en Node.js, como `readline` y `stream`, que pueden ser útiles en situaciones donde el rendimiento y la eficiencia de memoria son críticos, como cuando se leen archivos de texto muy grandes. 

La formtación específica de cómo se lee el archivo - línea por línea, o todo de una sola vez - puede variar dependiendo de tus necesidades. 

## Ver También:

1. [Documentación Node.js fs](https://nodejs.org/api/fs.html)
2. [Node.js readline en la documentación oficial](https://nodejs.org/api/readline.html)
3. [Cómo trabajar con archivos de texto en JavaScript](https://developer.mozilla.org/es/docs/Learn/JavaScript/Guide/Working_with_text_files)