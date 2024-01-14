---
title:    "TypeScript: Creando un archivo temporal"
keywords: ["TypeScript"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/es/typescript/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Por qué

No es raro encontrarse en situaciones en las que necesitamos almacenar información temporalmente mientras se ejecuta un programa. Para ello, es conveniente aprender cómo crear y manejar archivos temporales en TypeScript.

## Cómo hacerlo

La creación de archivos temporales en TypeScript es bastante sencilla. Primero, importamos el módulo fs (file system) y utilizamos su método `writeFile` para crear el archivo temporal. Veamos un ejemplo:

```TypeScript
import * as fs from 'fs';

fs.writeFile('temp.txt', 'Este es un archivo temporal', (err) => {
    if (err) throw err;
    console.log('Archivo temporal creado satisfactoriamente');
});

```

Este código creará un archivo llamado "temp.txt" en la ubicación donde se está ejecutando el programa. El segundo parámetro del método `writeFile` es el contenido del archivo, en este caso simplemente un string. También se puede pasar un objeto o un array.

Una vez que se ejecuta este código, podemos ver el archivo temporal creado y su contenido en nuestro sistema de archivos.

## Deep Dive

El método `writeFile` utilizado en el ejemplo anterior también acepta una variedad de opciones para ajustar cómo se crea y maneja el archivo temporal. Por ejemplo, se puede especificar el formato de codificación del archivo o incluso establecer si se debe sobrescribir el archivo si ya existe.

Además, en lugar de utilizar el método `writeFile`, también se puede utilizar el método `mkdtemp` para crear un directorio temporal en lugar de un archivo. Este método es especialmente útil si se necesita almacenar varios archivos o datos relacionados en un solo lugar.

## Ver también

- Documentación oficial de fs en TypeScript: https://www.typescriptlang.org/docs/handbook/working-with-files.html
- Ejemplos de código de manejo de archivos temporales: https://stackoverflow.com/questions/28502023/how-to-create-a-temporary-file-in-node-js