---
title:    "TypeScript: Comprobando si existe un directorio."
keywords: ["TypeScript"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/es/typescript/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Por qué

Verificar si un directorio existe es una tarea importante en la programación. A veces, antes de crear un nuevo directorio o realizar operaciones en un directorio específico, es necesario asegurarse de que realmente existe. De esta manera, podemos evitar errores y problemas en nuestro código.

## Cómo hacerlo

Para comprobar si un directorio existe en TypeScript, podemos utilizar la función `readdir()` del módulo `fs`. Esta función se encarga de leer el contenido de un directorio y devuelve un array con los nombres de los archivos y subdirectorios que contiene.

```TypeScript
import fs from 'fs';

const directory = './ejemplo';

fs.readdir(directory, (err, files) => {
  if (err) {
    console.log(`El directorio ${directory} no existe.`);
  } else {
    console.log(`El directorio ${directory} existe y contiene los siguientes archivos:`);
    for (const file of files) {
      console.log(file);
    }
  }
});
```

Si el directorio no existe, `readdir()` devolverá un error, por lo que podemos utilizar una sentencia `if` para manejar este caso y mostrar un mensaje al usuario. En caso de que el directorio exista, podemos iterar sobre el array de archivos y mostrarlos en consola.

## Profundizando

Además de utilizar la función `readdir()`, también podemos utilizar el método `existsSync()` del módulo `fs` para comprobar si un directorio existe. Este método devuelve un valor booleano `true` si el directorio existe y `false` si no existe.

```TypeScript
import fs from 'fs';

const directory = './ejemplo';

if (fs.existsSync(directory)) {
  console.log(`El directorio ${directory} existe.`);
} else {
  console.log(`El directorio ${directory} no existe.`);
}
```

Es importante tener en cuenta que esta función bloquea la ejecución del código hasta que se complete la comprobación, lo que puede ser un problema en aplicaciones en tiempo real. Por lo tanto, es recomendable utilizar `readdir()` en su lugar.

## Ver también

- [Documentación de Node.js sobre el módulo fs](https://nodejs.org/dist/latest-v14.x/docs/api/fs.html)
- [Método readdir() en TypeScript Handbook](https://www.typescriptlang.org/docs/handbook/file-system-utilities.html#readdir)