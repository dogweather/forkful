---
title:                "Creando un archivo temporal"
html_title:           "Arduino: Creando un archivo temporal"
simple_title:         "Creando un archivo temporal"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/typescript/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## ¿Qué y por qué?
Crear un archivo temporal implica generar un archivo para almacenamiento transitorio e intercambio de datos. Como programadores, hacemos esto para gestionar grandes cantidades de información, especialmente en operaciones que requieren manejo de memoria eficiente.

## ¿Cómo se hace?
TypeScript, a diferencia de Node.js, no tiene una función incorporada para crear archivos temporales. Sin embargo, podemos hacer uso de paquetes externos como `tmp-promise`. Asegúrate de instalarlo con `npm install tmp-promise`.

Aquí tenemos un ejemplo simple:
```typescript
import { file } from 'tmp-promise';

async function createTempFile() {
    const { path, fd } = await file({ prefix: 'myTemp-', postfix: '.txt' });
    console.log(path); // imprime la ruta al archivo temporal en consola
}

createTempFile();
```
La salida será la ruta al archivo temporal creado.

## Profundizando
TypeScript, lanzado en 2012, es en realidad un superconjunto de JavaScript que agrega tipos estáticos a la lengua. Por tanto, al igual que JavaScript, no ofrece una solución directamente integrada para crear archivos temporales.

En cuanto a las alternativas, además de `tmp-promise`, puedes considerar el uso de `tempy` y `temp`. Ambos ofrecen interfaces promesas, y `temp` también ofrece un método de seguimiento para limpiar todos los archivos temporales creados al final de tu programa.

En cuanto a cómo funciona, `tmp-promise` y similares utilizan funciones de bajo nivel del sistema operativo para generar nombres de archivos únicos. Estos archivos se crean en directorios designados para el almacenamiento temporal en tu sistema.

## Más información
- Documentación oficial de `tmp-promise`: https://www.npmjs.com/package/tmp-promise
- Alternativas como `tempy`: https://www.npmjs.com/package/tempy y `temp`: https://www.npmjs.com/package/temp
- Para entender a fondo cómo el sistema operativo maneja los archivos temporales, prueba el sistema de archivos `fs` de Node.js: https://nodejs.org/api/fs.html