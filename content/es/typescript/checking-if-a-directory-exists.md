---
title:                "Comprobando si existe un directorio"
html_title:           "TypeScript: Comprobando si existe un directorio"
simple_title:         "Comprobando si existe un directorio"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/typescript/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## ¿Qué y por qué?

Comprobar si un directorio existe es una comprobación que realizan los programadores para asegurarse de que un directorio específico existe en su sistema antes de realizar cualquier operación de lectura o escritura en él. Esto evita posibles errores o fallos en el código.

## Cómo:

```TypeScript
if (fs.existsSync(path)) {
  console.log('El directorio existe');
} else {
  console.log('El directorio no existe');
}
```

El código anterior utiliza la función `existsSync()` del módulo `fs` de Node.js para comprobar si el directorio especificado en la variable `path` existe en el sistema. Si es así, se imprimirá "El directorio existe", de lo contrario se imprimirá "El directorio no existe". Este es solo uno de los muchos métodos que se pueden utilizar para realizar esta comprobación en TypeScript.

## Profundizando:

En el pasado, los programadores solían usar la función `stat()` de C para comprobar la existencia de un directorio. Sin embargo, esta función no era compatible con todos los sistemas operativos y tenía algunos inconvenientes, como ser menos eficiente y menos seguro que los métodos utilizados actualmente.

Además, también se pueden utilizar librerías externas como `fs-extra` o `fs-jetpack` para realizar esta comprobación en lugar de utilizar los métodos nativos de Node.js.

Desde una perspectiva de implementación, los métodos utilizados para comprobar la existencia de un directorio suelen realizar llamadas al sistema operativo para obtener información sobre el directorio y luego devolver un resultado en función de esa información.

## Ver también:

- Documentación oficial de Node.js sobre el módulo `fs`: https://nodejs.org/api/fs.html
- Librería `fs-extra`: https://www.npmjs.com/package/fs-extra
- Librería `fs-jetpack`: https://www.npmjs.com/package/fs-jetpack