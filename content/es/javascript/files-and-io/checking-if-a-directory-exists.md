---
title:                "Comprobando si un directorio existe"
aliases:
- /es/javascript/checking-if-a-directory-exists/
date:                  2024-02-03T19:07:37.494989-07:00
model:                 gpt-4-0125-preview
simple_title:         "Comprobando si un directorio existe"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/javascript/checking-if-a-directory-exists.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Qué y Por qué?
Verificar si un directorio existe en JavaScript es esencial para las tareas de manipulación de archivos, permitiendo a los scripts verificar la presencia del directorio antes de leerlo o escribir en él. Esta operación previene errores y asegura una ejecución de programa más fluida, particularmente en aplicaciones que manejan archivos o directorios de manera dinámica basándose en la entrada del usuario o fuentes de datos externas.

## Cómo hacerlo:
En Node.js, dado que JavaScript en sí mismo no tiene acceso directo al sistema de archivos, el módulo `fs` es típicamente utilizado para tales operaciones. Aquí tienes una manera simple de verificar si un directorio existe utilizando `fs.existsSync()`:

```javascript
const fs = require('fs');

const directoryPath = './sample-directory';

// Verificar si el directorio existe
if (fs.existsSync(directoryPath)) {
  console.log('El directorio existe.');
} else {
  console.log('El directorio no existe.');
}
```
**Salida de Ejemplo:**
```
El directorio existe.
```
O, para un enfoque asíncrono no bloqueante, utiliza `fs.promises` con `async/await`:

```javascript
const fs = require('fs').promises;

async function checkDirectory(directoryPath) {
  try {
    await fs.access(directoryPath);
    console.log('El directorio existe.');
  } catch (error) {
    console.log('El directorio no existe.');
  }
}

checkDirectory('./sample-directory');
```
**Salida de Ejemplo:**
```
El directorio existe.
```

Para proyectos que hacen uso intensivo de operaciones de archivos y directorios, el paquete `fs-extra`, una extensión del módulo nativo `fs`, ofrece métodos adicionales convenientes. Así es cómo puedes lograr lo mismo con `fs-extra`:

```javascript
const fs = require('fs-extra');

const directoryPath = './sample-directory';

// Verificar si el directorio existe
fs.pathExists(directoryPath)
  .then(existe => console.log(existe ? 'El directorio existe.' : 'El directorio no existe.'))
  .catch(err => console.error(err));
```
**Salida de Ejemplo:**
```
El directorio existe.
```

Este enfoque permite un código limpio y legible que se integra a la perfección con las prácticas modernas de JavaScript.
