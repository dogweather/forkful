---
title:                "TypeScript: Redacción de un archivo de texto"
programming_language: "TypeScript"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/typescript/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Por qué escribir un archivo de texto

Escribir un archivo de texto es una práctica común en programación, ya que permite almacenar información de manera estructurada y accesible. Además, utilizar archivos de texto es una forma sencilla de compartir datos entre distintas aplicaciones.

## Cómo hacerlo

Para escribir un archivo de texto en TypeScript, podemos utilizar el módulo `fs` de Node.js. Primero, debemos importar este módulo en nuestro archivo:

```
import * as fs from 'fs';
```

Luego, podemos utilizar la función `writeFile()` para crear un nuevo archivo de texto y escribir en él. Por ejemplo, podemos crear un archivo llamado "ejemplo.txt" con el siguiente contenido:

```
fs.writeFile('ejemplo.txt', '¡Hola mundo!', (error) => {
  if (error) throw error;
  console.log('¡Archivo de texto creado exitosamente!');
});
```

Al ejecutar este código, deberíamos ver el mensaje de éxito en nuestra consola. Y si abrimos el archivo "ejemplo.txt", deberíamos ver el texto "¡Hola mundo!" escrito en él.

## Un vistazo más profundo

Además de simplemente escribir en un archivo de texto, podemos realizar otras acciones con el módulo `fs`, como leer y actualizar archivos existentes. También es importante tener en cuenta que al escribir en un archivo de texto, debemos considerar la codificación de caracteres utilizada para asegurarnos de que nuestro texto se vea correctamente.

## Ver también

- Documentación oficial de TypeScript para el módulo `fs`: https://www.typescriptlang.org/docs/handbook/nodejs.html
- Tutorial sobre cómo escribir y guardar archivos de texto en TypeScript: https://dev.to/waqasabbasi/how-to-read-and-write-files-in-typescript-45pe