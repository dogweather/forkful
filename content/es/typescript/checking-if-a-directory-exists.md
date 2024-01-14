---
title:    "TypeScript: Comprobando si existe un directorio."
keywords: ["TypeScript"]
---

{{< edit_this_page >}}

## ¿Por qué deberías verificar si un directorio existe?

Si estás trabajando en un proyecto de TypeScript que involucra la manipulación de archivos y directorios, es posible que necesites verificar si un directorio existe antes de realizar ciertas operaciones. Esto puede ayudarte a evitar errores y asegurarte de que tu código se esté ejecutando correctamente.

## Cómo hacerlo

En TypeScript, puedes verificar si un directorio existe utilizando el método `exists` de la clase `fs` (file system). Este método toma como parámetro el path del directorio que quieres verificar y devuelve un valor booleano que indica si el directorio existe o no.

```TypeScript
import * as fs from "fs";

if (fs.exists("ruta/al/directorio")) {
    console.log("¡El directorio existe!");
} else {
    console.log("El directorio no existe");
}
```

## Una mirada más profunda

Al utilizar el método `exists`, es importante tener en cuenta que puede devolver `false` incluso si el directorio existe. Esto puede suceder si tu aplicación no tiene permisos para acceder al directorio. Por lo tanto, es recomendable utilizar también el método `access` de la clase `fs` para verificar si tienes permisos de acceso al directorio antes de intentar acceder a él.

```TypeScript
import * as fs from "fs";

fs.access("ruta/al/directorio", (error) => {
    if (error) {
        console.log("No tienes permisos de acceso al directorio");
    } else {
        console.log("Puedes acceder al directorio");
    }
})
```

## Ver también

- [Documentación de fs.exists en TypeScript](https://www.typescriptlang.org/docs/handbook/filesystem.html#exists)
- [Documentación de fs.access en TypeScript](https://www.typescriptlang.org/docs/handbook/filesystem.html#access)
- [Tutorial de TypeScript para trabajar con archivos y directorios](https://www.digitalocean.com/community/tutorials/typescript-working-with-files-and-directories)