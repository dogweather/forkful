---
title:                "TypeScript: Comprobando si un directorio existe."
simple_title:         "Comprobando si un directorio existe."
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/typescript/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Por qué
Antes de comenzar a verificar la existencia de un directorio en TypeScript, es importante entender por qué necesitamos hacerlo. Comprobar si un directorio existe es una tarea común en la programación, ya que permite a los desarrolladores asegurarse de que un directorio necesario para su aplicación esté presente antes de continuar con el código.

## Cómo hacerlo
Para verificar si un directorio existe en TypeScript, podemos usar la función `existsSync()` del módulo `fs`, que verifica la existencia de un archivo o directorio sincrónicamente. Primero, debemos importar el módulo `fs` en nuestro archivo de TypeScript:

```typescript
import * as fs from 'fs';
```

Luego, podemos usar la función `existsSync()` pasando la ruta del directorio que queremos verificar como argumento. Esta función devolverá un valor booleano `true` si el directorio existe y `false` si no existe.

```typescript
const directoryExists = fs.existsSync('/ruta/del/directorio');

console.log(directoryExists); // Output: true o false
```

## Profundizando
La función `existsSync()` utiliza el método `statSync()` para determinar la existencia del directorio. Este método devuelve un objeto `fs.Stats` que contiene información sobre el archivo o directorio especificado, incluyendo un atributo `isDirectory()` que nos indica si se trata de un directorio o no.

Si queremos realizar acciones específicas si un directorio existe o no, podemos usar una declaración `if` junto con el método `isDirectory()` para verificar eso.

```typescript
if (fs.existsSync('/ruta/del/directorio')) {
    const directoryStats = fs.statSync('/ruta/del/directorio');
    if (directoryStats.isDirectory()) {
        // Hacer algo si es un directorio
    } else {
        // Hacer algo si no es un directorio
    }
}
```

## Ver también
- [Documentación de fs.existsSync()](https://nodejs.org/api/fs.html#fs_fs_existssync_path)
- [Documentación de fs.statSync()](https://nodejs.org/api/fs.html#fs_fs_stats_path_options)
- [Documentación de fs.Stats](https://nodejs.org/api/fs.html#fs_class_fs_stats)

¡Ahora estás listo para verificar la existencia de un directorio en TypeScript! Espero que este artículo te haya sido útil. ¡Hasta la próxima!