---
title:    "TypeScript: Leyendo un archivo de texto"
keywords: ["TypeScript"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/es/typescript/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Por qué
Algunas veces, como programadores, necesitamos leer y procesar información almacenada en un archivo de texto. Puede ser para extraer datos importantes, realizar cálculos, o simplemente para visualizar la información de una manera más legible. En este artículo, te mostraremos cómo leer un archivo de texto en TypeScript y trabajar con su contenido.

## Cómo hacerlo
Para leer un archivo de texto en TypeScript, primero necesitamos importar el módulo "fs" que nos permitirá interactuar con los archivos del sistema. A continuación, utilizaremos la función "readFile" para leer un archivo específico y almacenar su contenido en una variable. Por ejemplo:

```TypeScript
import * as fs from 'fs';

let data: string = fs.readFileSync('archivo.txt', 'utf8');
console.log(data);
```

En este caso, estamos utilizando la función "readFileSync" que sincroniza la lectura del archivo, lo que significa que nuestro programa esperará hasta que el archivo se haya leído completamente antes de continuar ejecutándose. También estamos especificando que queremos que el contenido del archivo se almacene en una variable de cadena.

Ahora que tenemos el contenido del archivo almacenado en una variable, podemos manipularlo de diferentes maneras. Por ejemplo, podemos convertir el texto en un arreglo utilizando el método "split" y especificando un delimitador. Por ejemplo:

```TypeScript
let arregloTexto: string[] = data.split('\n');
console.log(arregloTexto);
```

Esto dividirá el contenido del archivo en un arreglo, utilizando el salto de línea como separador. También podemos utilizar el método "replace" para reemplazar ciertas palabras o caracteres en el texto. Por ejemplo:

```TypeScript
let nuevoTexto: string = data.replace('hola', 'hola mundo');
console.log(nuevoTexto);
```

Estas son solo algunas de las muchas posibilidades que tenemos al leer y manipular archivos de texto en TypeScript.

## Profundizando
Ahora que hemos visto cómo leer un archivo de texto en TypeScript y trabajar con su contenido, hablemos un poco más sobre cómo el módulo "fs" nos permite interactuar con los archivos del sistema. Este módulo nos proporciona diferentes funciones para trabajar con archivos, como por ejemplo:

- `readFile`: como vimos anteriormente, esta función nos permite leer un archivo de manera sincronizada o asincrónica.
- `exists`: nos permite verificar si un archivo existe en una ruta específica.
- `writeFile`: nos permite escribir en un archivo, ya sea creándolo si no existe o sobrescribiendo su contenido si ya existe.
- `unlink`: elimina un archivo de una ruta específica.

Además, el módulo "fs" también nos permite trabajar con el directorio en el que se encuentra nuestro archivo, utilizando funciones como `readdir` (para leer los archivos en un directorio), `mkdir` (para crear un nuevo directorio) y `rmdir` (para eliminar un directorio).

Es importante tener en cuenta que cuando trabajamos con archivos y directorios, debemos tener permisos adecuados en nuestro sistema para poder leer, escribir o eliminarlos.

## Ver también
- Documentación oficial del módulo "fs" en TypeScript: https://nodejs.org/api/fs.html
- Guía rápida para leer y escribir archivos en TypeScript: https://www.digitalocean.com/community/tutorials/typescript-reading-writing-files
- Ejemplos de código para trabajar con archivos en TypeScript: https://github.com/DefinitelyTyped/DefinitelyTyped/tree/master/types/node/fs