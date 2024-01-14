---
title:                "Javascript: Leer un archivo de texto"
simple_title:         "Leer un archivo de texto"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/javascript/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Por qué
La lectura de archivos de texto es una tarea esencial en la programación. Ya sea que estés extrayendo datos de un archivo CSV o leyendo un archivo de configuración, saber cómo leer un archivo de texto puede ser muy útil.

## Cómo hacerlo
Para leer un archivo de texto en Javascript, podemos utilizar la función `readFileSync()` del módulo `fs`. Esta función acepta dos parámetros: la ruta del archivo y un objeto `options` que especifica la codificación del archivo.

Por ejemplo, si queremos leer un archivo llamado "datos.csv" que está en la misma carpeta que nuestro archivo Javascript, podemos hacerlo de la siguiente manera:

```Javascript
const fs = require('fs');

const datos = fs.readFileSync('datos.csv', { encoding: 'utf-8' });
console.log(datos);
```

Este código leerá el archivo y lo almacenará en la variable `datos`, que luego podemos utilizar para manipular o mostrar los datos.

## Profundizando
Hay varias formas de leer un archivo de texto en Javascript, dependiendo de tus necesidades. Por ejemplo, si necesitas leer un archivo de gran tamaño, puede ser más eficiente utilizar `createReadStream()` en lugar de `readFileSync()`. También puedes utilizar el módulo `path` para trabajar con rutas relativas y absolutas de archivos.

Además, es importante tener en cuenta la codificación del archivo que estás leyendo. Si utilizas `readFileSync()` sin especificar la codificación, el resultado puede ser inexacto.

## Ver también
- [Documentación de la función `readFileSync()`](https://nodejs.org/api/fs.html#fs_fs_readfilesync_path_options)
- [Documentación del módulo `path`](https://nodejs.org/api/path.html)
- [Tutorial de lectura de archivos en Javascript](https://www.w3schools.com/nodejs/nodejs_filesystem.asp)