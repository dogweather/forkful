---
title:                "Elm: Comprobando si existe un directorio"
simple_title:         "Comprobando si existe un directorio"
programming_language: "Elm"
category:             "Elm"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/elm/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Por qué

A medida que avanzamos en nuestro camino de aprendizaje de Elm, es importante tener en cuenta una funcionalidad básica como la comprobación de si un directorio existe. Esta es una habilidad importante, ya que nos permite crear un código más robusto y preparado para cualquier situación. En esta publicación, exploraremos por qué es importante verificar si un directorio existe y cómo podemos hacerlo en Elm.

## Cómo hacerlo

Antes de comenzar a escribir código, es importante comprender cómo funciona la comprobación de directorios en Elm. Podemos hacerlo utilizando la función `directoryExists` del módulo `File.System`. Esta función devuelve un valor `Bool` que indica si el directorio especificado existe o no.

Veamos un ejemplo de código para verificar si el directorio "mi_directorio" existe en la raíz del archivo:

```Elm
import File.System exposing (directoryExists)

myDirectoryExists : Bool
myDirectoryExists =
  directoryExists "mi_directorio"
```

El valor devuelto por `myDirectoryExists` será `True` si el directorio existe y `False` si no existe. Podemos utilizar este valor en nuestro código para tomar decisiones o realizar otras acciones.

Ahora, supongamos que queremos imprimir un mensaje diferente dependiendo de si el directorio existe o no. Podemos hacerlo utilizando una expresión `if-then-else`:

```Elm
if myDirectoryExists then
  "El directorio existe"
else
  "El directorio no existe"
```

Con esto, podemos escribir todo lo necesario para verificar si un directorio existe en nuestro código.

## Profundizando

Ahora que sabemos cómo utilizar la función `directoryExists`, es importante tener en cuenta algunas cosas a tener en cuenta al usarla.

En primer lugar, es importante tener en cuenta que esta función solo comprueba la existencia de directorios, no de archivos. Si queremos comprobar si un archivo existe, debemos utilizar la función `fileExists` del mismo módulo.

También es importante recordar que esta función solo comprueba la existencia de directorios en la raíz del archivo. Si queremos verificar si un directorio existe en una ubicación diferente, debemos incluir la ruta completa del directorio en la función.

## Ver también

- [Documentación oficial de Elm para la función `directoryExists`](https://package.elm-lang.org/packages/elm/file/latest/File-System#directoryExists)
- [Artículo de programación en español sobre la comprobación de directorios en Elm](https://programacion.net/articulo/comprobante_existencia_directorio_elm_1917)