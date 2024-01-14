---
title:                "TypeScript: Comprobando si existe un directorio"
programming_language: "TypeScript"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/typescript/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Porqué

¿Alguna vez te has encontrado en una situación en la que necesitas saber si una carpeta existe en tu sistema de archivos? Bueno, en esta publicación de blog aprenderás cómo hacerlo usando TypeScript.

## Cómo Hacerlo

Para verificar si una carpeta existe en TypeScript, podemos usar la librería incorporada `fs` y su método `existsSync`. Este método acepta una ruta de la carpeta como parámetro y devuelve un valor booleano indicando si la carpeta existe o no.

```TypeScript
import fs from "fs";

const exists = fs.existsSync("mi_carpeta");

// Salida: true si la carpeta existe, false si no existe
console.log(exists);
```

¡Y así de simple es! Con sólo unas pocas líneas de código, podemos verificar la existencia de una carpeta en nuestro sistema de archivos.

## Deep Dive

Si deseas profundizar un poco más en el tema, también podemos usar el método `readdirSync` de la librería `fs` para obtener una lista de todas las carpetas y archivos en una ruta específica y luego comprobar si nuestra carpeta objetivo está en esa lista.

```TypeScript
import fs from "fs";

const folderList = fs.readdirSync("mi_ruta");

// Verificamos si "mi_carpeta" está en la lista
if (folderList.includes("mi_carpeta")) {
  // La carpeta existe
  console.log("La carpeta existe en la ruta dada");
} else {
  // La carpeta no existe
  console.log("La carpeta no existe en la ruta dada");
}
```

También podemos usar el método `statSync` para obtener información detallada sobre una ruta, incluyendo si es una carpeta o no. Luego podemos verificar si la propiedad `isDirectory()` es verdadera para determinar si nuestra ruta realmente es una carpeta o no.

```TypeScript
import fs from "fs";

const stats = fs.statSync("mi_ruta");

// Verificamos si es una carpeta
if (stats.isDirectory()) {
  // La ruta es una carpeta
  console.log("La ruta dada es una carpeta");
} else {
  // La ruta no es una carpeta
  console.log("La ruta dada no es una carpeta");
}
```

¡Con todas estas opciones a tu disposición, ahora puedes verificar la existencia de una carpeta en TypeScript de manera fácil y eficiente!

## Ver También

- [Documentación oficial de node.js para fs.existsSync](https://nodejs.org/api/fs.html#fs_fs_existssync_path)
- [Documentación oficial de node.js para fs.readdirSync](https://nodejs.org/api/fs.html#fs_fs_readdirsync_path_options)
- [Documentación oficial de node.js para fs.statSync](https://nodejs.org/api/fs.html#fs_fs_statsync_path_options)