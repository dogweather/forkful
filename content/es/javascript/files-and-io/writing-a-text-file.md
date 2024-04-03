---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:28:13.844130-07:00
description: "Escribir un archivo de texto en JavaScript a menudo se refiere a crear\
  \ y guardar datos en un formato simple y legible para el registro, exportaci\xF3\
  n de\u2026"
lastmod: '2024-03-13T22:44:59.476740-06:00'
model: gpt-4-0125-preview
summary: "Escribir un archivo de texto en JavaScript a menudo se refiere a crear y\
  \ guardar datos en un formato simple y legible para el registro, exportaci\xF3n\
  \ de entradas del usuario o fines de configuraci\xF3n."
title: Escribiendo un archivo de texto
weight: 24
---

## Cómo hacerlo:
En un entorno de Node.js, puedes usar el módulo integrado `fs` (Sistema de Archivos) para escribir archivos de texto. Este ejemplo demuestra cómo escribir texto en un archivo de manera asíncrona:

```javascript
const fs = require('fs');

const data = '¡Hola, Mundo! Este es el texto para ser escrito en un archivo.';

fs.writeFile('ejemplo.txt', data, (err) => {
  if (err) {
    throw err;
  }
  console.log('El archivo ha sido escrito.');
});
```

Salida de muestra:
```
El archivo ha sido escrito.
```

Para la escritura de archivos sincrónica, usa `writeFileSync`:
```javascript
try {
  fs.writeFileSync('ejemplo.txt', data);
  console.log('El archivo ha sido escrito.');
} catch (error) {
  console.error('Error al escribir el archivo:', error);
}
```

En los navegadores web modernos, la API de Acceso al Sistema de Archivos introduce la capacidad de leer y escribir archivos. Sin embargo, su uso está sujeto a permisos del usuario. Esta es la forma de crear y escribir en un archivo:

```javascript
if ('showSaveFilePicker' in window) {
  const handle = await window.showSaveFilePicker();
  const writable = await handle.createWritable();
  await writable.write('¡Hola, Mundo! Este es el texto de escritura de archivos del navegador.');
  await writable.close();
}
```

Para escenarios más complejos o al trabajar con archivos grandes, podrías optar por librerías de terceros como `FileSaver.js` para navegadores:

```html
<script src="https://cdnjs.cloudflare.com/ajax/libs/FileSaver.js/2.0.2/FileSaver.min.js"></script>
<script>
  const blob = new Blob(["¡Hola, Mundo! Este es el texto de FileSaver.js."], {type: "text/plain;charset=utf-8"});
  saveAs(blob, "ejemplo.txt");
</script>
```

Recuerda, escribir archivos en el lado del cliente (en navegadores) está restringido debido a preocupaciones de seguridad, y cualquier operación que requiera guardar en el disco local del usuario generalmente requerirá su permiso explícito.
