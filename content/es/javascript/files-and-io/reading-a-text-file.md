---
title:                "Lectura de un archivo de texto"
aliases:
- /es/javascript/reading-a-text-file/
date:                  2024-01-20T17:54:48.661598-07:00
model:                 gpt-4-1106-preview
simple_title:         "Lectura de un archivo de texto"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/javascript/reading-a-text-file.md"
---

{{< edit_this_page >}}

## ¿Qué y Por Qué?
Leer un archivo de texto en JavaScript es simplemente acceder al contenido almacenado en un archivo de texto (.txt) y trabajar con él en tu programa. Los programadores lo hacen para manipular datos, configuraciones, o importar información externa sin necesidad de meter todo a mano en el código.

## Cómo hacerlo:
JavaScript, especialmente en Node.js, hace que leer archivos de texto sea un juego de niños. Aquí hay un ejemplo sencillo usando `fs`, el módulo de sistema de archivos incorporado en Node.js:

```javascript
const fs = require('fs');

// Sincrono
const contenido = fs.readFileSync('ejemplo.txt', 'utf-8');
console.log(contenido);

// Asincrono
fs.readFile('ejemplo.txt', 'utf-8', (err, contenido) => {
  if (err) {
    console.error(err);
    return;
  }
  console.log(contenido);
});
```

Output sincrono:
```
Hola, esto es un texto de ejemplo!
```

Output asincrono (será el mismo, pero dependerá de cuándo termine la operación asincrónica):
```
Hola, esto es un texto de ejemplo!
```

## Deep Dive
Antiguamente, leer un archivo podría significar una operación compleja que requería manejar buffers y entender cómo el sistema operativo manejaba los archivos. Hoy, Node.js abstrae esas complejidades con su módulo `fs`.

Alternativas existen, incluyendo el uso de `fs.promises` para trabajar con promesas nativas en vez de callbacks, lo que puede resultar en un código más claro y moderno:

```javascript
const fs = require('fs').promises;

async function leerArchivo() {
  try {
    const contenido = await fs.readFile('ejemplo.txt', 'utf-8');
    console.log(contenido);
  } catch (err) {
    console.error(err);
  }
}

leerArchivo();
```

En el navegador, las cosas funcionan diferente por cuestiones de seguridad y privacidad. Sin embargo, podemos leer archivos desde la entrada de un usuario usando `FileReader`:

```javascript
const input = document.createElement('input');
input.type = 'file';

input.onchange = e => {
  const file = e.target.files[0];
  const reader = new FileReader();

  reader.onload = () => {
    console.log(reader.result);
  };
  
  reader.readAsText(file);
};

document.body.appendChild(input);
```

En este caso, `FileReader` se encarga de procesar el archivo seleccionado y podemos manejar su contenido una vez que se dispara el evento `onload`.

## Ver También
- Documentación oficial de Node.js sobre el módulo `fs`: [Node.js fs module](https://nodejs.org/api/fs.html)
- Explicación detallada sobre `FileReader` en la Web API: [MDN FileReader](https://developer.mozilla.org/en-US/docs/Web/API/FileReader)
- Para entender las Promises en JavaScript: [MDN Promises](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Guide/Using_promises) 
- Información sobre operaciones de archivos en Buffer: [Node.js Buffer](https://nodejs.org/api/buffer.html)
