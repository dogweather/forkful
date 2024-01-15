---
title:                "Escribir un archivo de texto"
html_title:           "Javascript: Escribir un archivo de texto"
simple_title:         "Escribir un archivo de texto"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/javascript/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Por qué

Escribir un archivo de texto puede ser una tarea útil y necesaria para cualquier programador. No solo te permite guardar y almacenar información, sino que también es una forma sencilla de compartir datos con otros desarrolladores o equipos de trabajo.

## Cómo

Para escribir un archivo de texto en Javascript, podemos utilizar la función `fs.writeFileSync()` que nos permite crear un archivo, escribir contenido en él y guardarlo en una ubicación específica.

Por ejemplo, si queremos crear un archivo llamado `nombres.txt` con una lista de nombres separados por coma, podemos utilizar el siguiente código:

```Javascript
const fs = require('fs');
const nombres = ["María", "Juan", "Sofía"];
fs.writeFileSync("nombres.txt", nombres.join(","));
```

Este código primero importa el módulo `fs` que nos permite interactuar con el sistema de archivos, luego define una variable `nombres` con una lista de nombres y, finalmente, utiliza la función `writeFileSync()` para crear el archivo `nombres.txt` y escribir en él los nombres separados por coma.

El resultado del archivo `nombres.txt` será el siguiente:

```
María, Juan, Sofía
```

## Profundizando

La función `writeFileSync()` acepta tres parámetros: el nombre del archivo que queremos crear, el contenido que queremos escribir en él y una opción para especificar el formato en el que queremos guardar el archivo.

También podemos utilizar la función `writeFile()` en lugar de `writeFileSync()`, que acepta los mismos parámetros pero se ejecuta de forma asíncrona, lo que significa que no detiene la ejecución del programa mientras escribe el archivo.

Si queremos añadir contenido a un archivo que ya existe, podemos utilizar la función `appendFileSync()` que funciona de la misma manera que `writeFileSync()` pero agrega el contenido al final del archivo en lugar de reemplazarlo.

## Ver también

- [Documentación oficial de Node.js](https://nodejs.org/api/fs.html#fs_fs_writefilesync_file_data_options)
- [Guía de programación en Javascript](https://developer.mozilla.org/es/docs/Web/JavaScript/Guide)