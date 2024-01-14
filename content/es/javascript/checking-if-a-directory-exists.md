---
title:                "Javascript: Comprobando si existe un directorio"
simple_title:         "Comprobando si existe un directorio"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/javascript/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Por qué

Revisar si un directorio existe es una tarea esencial para cualquier programador de JavaScript. Esta comprobación nos permite asegurarnos de que el directorio que estamos intentando acceder existe antes de realizar cualquier acción en él. Además, nos ayuda a evitar errores y mejora la experiencia del usuario final.

## Cómo

Para revisar si un directorio existe en JavaScript, utilizamos la función ```fs.existsSync()```. Esta función toma como argumento la ruta del directorio que queremos comprobar y devuelve un valor booleano: ```true``` si el directorio existe y ```false``` si no existe.

Veamos un ejemplo de cómo usar esta función para verificar si un directorio llamado "archivos" existe en nuestro proyecto:

```Javascript
const fs = require("fs");
const directorio = "./archivos";

if (fs.existsSync(directorio)) {
  console.log("El directorio existe");
}
else {
  console.log("El directorio no existe");
}
```

En este ejemplo, estamos utilizando la función ```require()``` para importar el módulo "fs" de Node.js, que nos permite interactuar con los archivos del sistema. Luego, creamos una variable que almacena la ruta del directorio que queremos comprobar.

Dentro de un bloque ```if-else```, utilizamos la función ```fs.existsSync()``` para verificar si el directorio existe o no. Dependiendo del resultado, imprimimos un mensaje en la consola para informar al usuario.

## Deep Dive

Ahora que hemos visto cómo utilizar la función ```fs.existsSync()```, es importante mencionar algunos detalles importantes sobre esta comprobación.

En primer lugar, debemos tener en cuenta que esta función solo comprueba si un directorio existe o no. No nos dice si tenemos permisos para acceder a él o si está vacío. También es importante destacar que esta función solo funciona para directorios locales, no para directorios en línea (URL).

Otra cosa a tener en cuenta es que la ruta que pasamos como argumento debe ser una ruta absoluta, no una ruta relativa. Por ejemplo, en el ejemplo anterior, utilizamos la variable ```directorio``` para almacenar la ruta del directorio "archivos", pero si lo hubiéramos hecho como ```./archivos```, la comprobación no habría funcionado.

## See Also

Para obtener más información sobre cómo trabajar con archivos y directorios en JavaScript, asegúrese de revisar la documentación oficial de Node.js [aquí](https://nodejs.org/api/fs.html).

En caso de que quiera aprender más sobre rutas absolutas y relativas, recomendamos leer este artículo [aquí](https://www.freecodecamp.org/espanol/news/rutas-absolutas-y-rutas-relativas-en-un-sistema-de-archivos/).