---
title:                "TypeScript: Creando un archivo temporal"
simple_title:         "Creando un archivo temporal"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/typescript/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Por qué crear un archivo temporal en TypeScript

Crear un archivo temporal puede ser útil en ciertos escenarios, como por ejemplo cuando se necesita almacenar datos temporalmente durante la ejecución de una aplicación. También puede ser útil en aplicaciones que necesitan crear archivos de forma dinámica y no quieren sobrecargar el sistema con archivos innecesarios.

## Cómo crear un archivo temporal en TypeScript

Para crear un archivo temporal en TypeScript, podemos utilizar el módulo `fs` de Node.js. Este módulo nos permite trabajar con el sistema de archivos y nos proporciona un método llamado `mkdtempSync` para crear un directorio temporal en la ubicación especificada.

Vamos a ver un ejemplo de cómo crear y escribir en un archivo temporal utilizando TypeScript:

```typescript
// Importamos el módulo fs
import * as fs from 'fs';

// Creamos un directorio temporal
const tempDir = fs.mkdtempSync('/tmp/');

// Creamos un archivo dentro del directorio temporal
const tempFile = fs.createWriteStream(`${tempDir}/temp.txt`);

// Escribimos en el archivo
tempFile.write('¡Hola! Este es un archivo temporal.');

// Cerramos el archivo
tempFile.end();

// Imprimimos la ubicación del archivo
console.log(`Archivo temporal creado en: ${tempFile.path}`);
```

El código anterior creará un directorio temporal dentro de la carpeta `/tmp/` y dentro de ese directorio un archivo llamado `temp.txt`. Luego, escribirá el mensaje "¡Hola! Este es un archivo temporal." en ese archivo y nos mostrará la ruta completa del archivo.

## Profundizando en la creación de un archivo temporal en TypeScript

Cuando creamos un archivo temporal utilizando `fs.mkdtempSync()`, el sistema operativo le asignará un nombre generado aleatoriamente. Sin embargo, también podemos especificar un prefijo para el nombre del archivo temporal utilizando el parámetro `prefix`.

También podemos proporcionar una ruta de acceso relativa o absoluta para el directorio temporal en el que queremos crear el archivo, en lugar de utilizar la ubicación predeterminada `/tmp/`.

Otra forma de crear un archivo temporal es utilizando el método `tmpNameSync` del módulo `tmp`. Este método nos permite especificar el formato del nombre del archivo temporal utilizando el enmascaramiento del texto entre paréntesis, por ejemplo `tmp.tmpNameSync({ template: 'tempfile-XXXXXX' })` creará un archivo temporal con el formato "tempfile-XXXXXX", donde las X serán reemplazadas por caracteres alfanuméricos aleatorios.

## Ver también

- [Documentación del módulo fs de Node.js](https://nodejs.org/api/fs.html)
- [Documentación del módulo tmp de Node.js](https://www.npmjs.com/package/tmp)