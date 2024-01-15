---
title:                "Comprobando si existe un directorio"
html_title:           "Elm: Comprobando si existe un directorio"
simple_title:         "Comprobando si existe un directorio"
programming_language: "Elm"
category:             "Elm"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/elm/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Por qué
Hay varias razones por las cuales querríamos comprobar si un directorio existe en nuestro código de Elm. Por ejemplo, podemos querer asegurarnos de que una ruta de archivo específica es válida antes de intentar leer o escribir en ella, o tal vez necesitamos verificar si un directorio creado por el usuario ya existe antes de crear uno nuevo.

## Cómo hacerlo
Comprobar si un directorio existe en Elm es bastante sencillo. Utilizaremos la función `directoryExists` de la biblioteca `elm/file` para realizar la verificación.

Primero, debemos importar la biblioteca y la función en nuestro código:

```Elm
import File
import File.Directory exposing (directoryExists)
```

Luego, podemos llamar a la función `directoryExists` pasándole como argumento la ruta del directorio que queremos comprobar:

```Elm
directoryExists "ruta/del/directorio"
```

Esta función devolverá un valor `Task Bool`, que indica si el directorio existe o no. Podemos manejar este resultado utilizando la función `Task.map` y proporcionando una función para lidiar con el resultado de la tarea.

Por ejemplo, si queremos imprimir un mensaje indicando si el directorio existe o no, podemos hacerlo de la siguiente manera:

```Elm
let
    verificarDirectorio path =
        Task.map
            (\exists -> if exists then
                Debug.log "El directorio existe"
            else
                Debug.log "El directorio no existe"
            )
            (directoryExists path)
```

Y luego podemos llamar a esta función pasándole nuestra ruta de directorio:

```Elm
verificarDirectorio "ruta/del/directorio"
```

Si el directorio existe, veremos el mensaje "El directorio existe" en la consola. Si no existe, veremos el mensaje "El directorio no existe". Podemos utilizar la función `Debug.todo` en lugar de `Debug.log` para generar un error si el directorio no existe, en lugar de simplemente imprimir un mensaje informativo.

## Profundizando
La función `directoryExists` utiliza la API de FileSystem de JavaScript para determinar si un directorio existe en el sistema de archivos. Funciona en navegadores y en Node.js, pero no en entornos de servidor de Elm por motivos de seguridad.

Si necesitamos realizar esta comprobación en un entorno de servidor, podemos crear una API en JavaScript que realice la verificación por nosotros y luego llamar a esa función desde nuestro código de Elm.

Otra cosa a tener en cuenta es que esta función solo comprueba si el directorio existe en la ubicación especificada, no comprueba si el directorio está vacío o si tiene permisos de lectura o escritura. Si necesitamos realizar estas verificaciones, debemos hacerlo por separado utilizando otras funciones y bibliotecas.

## Ver también
- [Documentación oficial de Elm: biblioteca elm/file](https://package.elm-lang.org/packages/elm/file/latest/)
- [Página de GitHub de elm/file](https://github.com/elm/file)
- [Repositorio de ejemplo en GitHub que utiliza elm/file](https://github.com/billstclair/elm-file/blob/master/examples/full-dirs-demo/src/Main.elm)