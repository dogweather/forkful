---
title:                "Creando un archivo temporal"
html_title:           "Arduino: Creando un archivo temporal"
simple_title:         "Creando un archivo temporal"
programming_language: "Elm"
category:             "Elm"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/elm/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## ¿Qué & Por qué?

Crear un archivo temporal implica generar un archivo de uso corto durante la ejecución de un programa. Los programadores lo hacen para almacenar datos de manera transitoria, especialmente útil cuando se manejan grandes volúmenes de información o para guardar el estado en operaciones complejas.

## Cómo hacerlo:

Debido a las características de la interacción de Elm con el sistema operativo, Elm en sí no permite la creación directa de archivos temporales. Pero podemos interactuar a través de sus funciones externas (port en elm) para lograr esta tarea en el entorno Javascript. Aquí te mostramos un ejemplo.

```Elm
port module TempFile exposing (..)

type alias Flag =
  { flag : String }

type alias FileOptions =
  { encoding : String
  , flag : Flag
  }

port tempFile : FileOptions -> (Result String String -> msg) -> Cmd msg
```

En este ejemplo, hemos creado un módulo llamado TempFile con una función `tempFile`. Esta función solicita opciones de archivo y envía una command (Cmd msg) que maneja la creación de un archivo temporal en el entorno Javascript.

Ahora aquí está la parte de Javascript,

```Javascript
app.ports.tempFile.subscribe((fileOptions) => {
  const tmp = require('tmp');
  tmp.file(fileOptions, function(err, path, fd, cleanupCallback) {
    if (err) throw err;
    console.log("File: ", path);
    console.log("Filedescriptor: ", fd);
  });
});
```

Asegúrate de importar el paquete 'tmp' en tu archivo Javascript.

## Profundizando

Elm prioriza la interacción segura con el sistema en lugar de proporcionar un acceso directo a las operaciones del sistema operativo, razón por la cual las operaciones de E/S de archivos a menudo se realizan a través de las puertas de enlace a Javascript. Alternativamente, podrías considerar un enfoque diferente para almacenar datos temporalmente, como almacenar en `localStorage` si estás en un entorno de navegador, o usar una base de datos en memoria si estás en un entorno de servidor.

Los detalles de implementación aquí mostrados son a nivel básico. La creación de archivos temporales puede conseguirse a través de diversas bibliotecas en Javascript y el manejo de errores, la limpieza de archivos y las llamadas a las funciones se pueden customizar según las necesidades.

## Ver También

- [Documentación oficial de Elm](http://elm-lang.org/)
- [Biblioteca 'tmp' en Javascript](https://www.npmjs.com/package/tmp)
- [Documentación de puertas de enlace (port) en Elm](https://guide.elm-lang.org/interop/ports.html)
- [Almacenamiento en el navegador con `localStorage`](https://developer.mozilla.org/es/docs/Web/API/Window/localStorage)