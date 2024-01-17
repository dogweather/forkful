---
title:                "Escribiendo un archivo de texto"
html_title:           "Javascript: Escribiendo un archivo de texto"
simple_title:         "Escribiendo un archivo de texto"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/javascript/writing-a-text-file.md"
---

{{< edit_this_page >}}

## ¿Qué y por qué?

Escribir un archivo de texto puede ser una tarea común para los programadores, pero ¿qué significa exactamente? En pocas palabras, escribir un archivo de texto es guardar información en un archivo que puede ser leído por un ser humano en formato de texto plano. Los programadores hacen esto para almacenar datos, configuraciones o cualquier otra información que necesiten para su programa.

## ¿Cómo hacerlo?

En Javascript, escribir un archivo de texto es algo sencillo. Primero, necesitamos incluir el módulo "fs" para tener acceso a las funciones de sistema de archivos. Luego, utilizamos la función "writeFile" para crear un archivo con el nombre que queramos y el contenido que deseamos. Aquí tienes un ejemplo de cómo hacerlo:

```Javascript
const fs = require('fs');

fs.writeFile('miArchivo.txt', 'Este es el contenido de mi archivo creado con Javascript', (err) => {
    if (err) throw err;
    console.log('¡Archivo creado correctamente!');
});
```

Si ejecutas este código, verás que se ha creado un archivo con el nombre "miArchivo.txt" en la misma ubicación donde se encuentra tu archivo Javascript. El contenido del archivo será "Este es el contenido de mi archivo creado con Javascript".

## Un vistazo más profundo

Para aquellos que quieran saber más sobre cómo escribir archivos de texto en Javascript, aquí hay algunos detalles adicionales. Esta función fue introducida en la versión 0.4.0 de Node.js y ha estado disponible en las versiones posteriores. Otra forma de crear un archivo de texto es utilizando el método "appendFile" en lugar de "writeFile". La diferencia entre ambos es que "writeFile" creará un archivo nuevo o sobrescribirá uno existente, mientras que "appendFile" añadirá el contenido al final del archivo ya existente.

## Ver también

Si quieres saber más sobre cómo escribir archivos de texto en Javascript, puedes consultar la documentación oficial de Node.js sobre el módulo "fs" (https://nodejs.org/api/fs.html). También puedes encontrar algunos tutoriales y ejemplos útiles en línea. ¡Sigue practicando y pronto podrás escribir archivos de texto en Javascript sin problemas!