---
title:    "Javascript: Creando un archivo temporal"
keywords: ["Javascript"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/es/javascript/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Por qué
En la programación de Javascript, a menudo nos encontramos con la necesidad de crear archivos temporales. Estos archivos pueden ser útiles para almacenar datos de forma temporal mientras se ejecuta un programa, o para realizar pruebas de código sin afectar los archivos permanentes. Aprender a crear archivos temporales puede ser muy útil para optimizar nuestro flujo de trabajo y evitar problemas en el futuro.

## Cómo crear archivos temporales en Javascript
Para crear un archivo temporal en Javascript, podemos utilizar la biblioteca integrada "fs". Primero, debemos importar la biblioteca con la sentencia `require`:
```Javascript
const fs = require('fs');
```
Luego, utilizaremos el método `writeFile` para crear y escribir en el archivo temporal:
```Javascript
fs.writeFile('temp.txt', 'Este es un archivo temporal', (err) => {
  if (err) throw err;
  console.log('Archivo temporal creado');
});
```
Este código creará un archivo llamado "temp.txt" y escribirá en él el texto "Este es un archivo temporal". Si queremos leer el contenido de nuestro archivo temporal, podemos utilizar el método `readFile`:
```Javascript
fs.readFile('temp.txt', 'utf8', (err, data) => {
  if (err) throw err;
  console.log(data);
});
```
El método `readFile` nos devolverá el contenido del archivo temporal en formato de texto.

## Profundizando en la creación de archivos temporales
La función `writeFile` nos permite especificar un parámetro opcional para indicar la codificación del archivo. Por defecto, utiliza "utf8", pero podemos cambiarlo según nuestras necesidades. Además, podemos utilizar el método `unlink` para eliminar el archivo temporal después de utilizarlo.

También es importante tener en cuenta que la creación de archivos temporales puede variar en diferentes sistemas operativos. Por ejemplo, en Windows, el nombre del archivo temporal puede incluir una ruta de archivo completa, mientras que en Linux puede ser simplemente un nombre aleatorio.

En resumen, la creación de archivos temporales puede ser una herramienta muy útil en la programación de Javascript, y es importante entender cómo funciona y cómo adaptarlo a nuestras necesidades específicas.

## Ver también
- [Documentación oficial de Node.js sobre la creación de archivos temporales](https://nodejs.org/api/fs.html#fs_fs_writefile_file_data_options_callback)
- [Artículo en Medium: Crear y leer archivos en Node.js usando la biblioteca fs](https://medium.com/swlh/creating-and-reading-files-in-node-js-using-the-fs-module-592a4b7a7e1#.pinu9ycnu)