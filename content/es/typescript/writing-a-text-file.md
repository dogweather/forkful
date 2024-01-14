---
title:    "TypeScript: Escribiendo un archivo de texto."
keywords: ["TypeScript"]
---

{{< edit_this_page >}}

## Por qué

Escribir un archivo de texto es una habilidad esencial para cualquier programador ya que permite almacenar y organizar información de manera estructurada. Además, es una buena práctica mantener comentarios y notas dentro de los archivos de código para facilitar su comprensión y colaboración en equipo.

## Cómo

Para escribir un archivo de texto en TypeScript, primero debemos crear una instancia de la clase "WriteStream" del módulo "fs" (file system). Luego, utilizando el método "write()", podemos escribir nuestro texto dentro del archivo. Por último, debemos cerrar el archivo con el método "close()" para que se guarde correctamente.

```TypeScript
import * as fs from "fs";

// creando una instancia de WriteStream y especificando el nombre del archivo
let archivo = fs.createWriteStream("ejemplo.txt");

// escribiendo texto en el archivo
archivo.write("Este es un ejemplo de cómo escribir un archivo de texto en TypeScript.");

// cerrando el archivo
archivo.close();
```

El código anterior creará un archivo de texto llamado "ejemplo.txt" y escribirá el texto especificado dentro de él.

## Profundizando

Además de escribir texto simple, también podemos utilizar el módulo "fs" para realizar otras funciones útiles al escribir un archivo de texto. Por ejemplo, podemos utilizar el método "appendFile()" para agregar texto a un archivo ya existente sin borrar su contenido anterior. También podemos utilizar el método "writeFileSync()" para escribir texto de manera sincrónica, lo que asegura que el archivo se guarde completamente antes de continuar con el código.

```TypeScript
import * as fs from "fs";

// agregando texto al archivo "ejemplo.txt" sin borrar su contenido anterior
fs.appendFile("ejemplo.txt", "\nEste texto se agregará en una nueva línea.", (error) => {
    if (error) {
        console.log(error);
    }
});

// escribiendo texto de manera sincrónica con writeFileSync
fs.writeFileSync("ejemplo.txt", "Este texto se guardará completamente antes de continuar con el código.");
```

Si deseamos escribir un archivo de texto con caracteres especiales como acentos o símbolos, podemos especificar la codificación "utf-8" en el método "createWriteStream()" para asegurarnos de que nuestro archivo se guarde correctamente.

## Ver también

- [Documentación de Node.js para el módulo "fs"](https://nodejs.org/api/fs.html)
- [Más información sobre la codificación "utf-8"](https://es.wikipedia.org/wiki/UTF-8)