---
title:    "Javascript: Comprobación de existencia de un directorio"
keywords: ["Javascript"]
---

{{< edit_this_page >}}

## Por qué

En el mundo de la programación, siempre es importante asegurarse de que nuestras acciones sean correctas y eficientes. Al trabajar con directorios en Javascript, una tarea común es verificar si un directorio existe antes de realizar cualquier operación. Esto nos permite evitar errores y nos ayuda a construir una lógica más sólida en nuestro código.

## Cómo hacerlo

Para verificar si un directorio existe en Javascript, podemos utilizar la función `existsSync` del módulo `fs` (file system). Esta función nos permite comprobar si un directorio o archivo existe en una ubicación específica. A continuación, se muestra un ejemplo de código y su respectivo resultado:

```Javascript
const fs = require('fs');

// Verificar si el directorio "documentos" existe en el escritorio
if (fs.existsSync('Desktop/documentos')) {
  console.log('El directorio existe en el escritorio');
} else {
  console.log('El directorio no existe en el escritorio');
}

// Verificar si el directorio "fotos" existe en la carpeta de usuario
if (fs.existsSync('C:/Usuarios/usuario/fotos')) {
  console.log('El directorio existe en la carpeta de usuario');
} else {
  console.log('El directorio no existe en la carpeta de usuario');
}
```

**Resultado:**

```
El directorio existe en el escritorio
El directorio no existe en la carpeta de usuario
```

## Profundizando

Al utilizar la función `existsSync`, es importante tener en cuenta que solo nos permite comprobar si un directorio o archivo existe en una ubicación específica. Esta función no nos dice si el directorio está vacío o si contiene archivos o subdirectorios.

Si necesitamos conocer más detalles sobre un directorio, podemos utilizar otras funciones del módulo `fs`, como `readdirSync`, que nos permite obtener una lista de los archivos y subdirectorios que contiene un directorio determinado.

También es importante mencionar que, al trabajar con rutas de directorios en Javascript, es recomendable utilizar el módulo `path`, ya que nos ayuda a manejar las rutas de una manera más eficiente y menos propensa a errores.

## Ver también

- [Documentación oficial de Node.js sobre la función `existsSync`](https://nodejs.org/api/fs.html#fs_fs_existssync_path)
- [Documentación oficial de Node.js sobre el módulo `path`](https://nodejs.org/api/path.html)