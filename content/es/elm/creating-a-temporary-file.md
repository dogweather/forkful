---
title:    "Elm: Creando un archivo temporal."
keywords: ["Elm"]
---

{{< edit_this_page >}}

## Por qué

Crear un archivo temporal puede ser útil en situaciones en las que se necesita almacenar datos temporalmente sin sobrescribir o afectar el archivo original. También puede ayudar a optimizar la gestión de memoria y mejorar el rendimiento del código.

## Cómo hacerlo

Crear un archivo temporal en Elm es sencillo utilizando la biblioteca *justgook/temporary*. Solo se necesita importar el módulo y utilizar la función `Temporary.file` para crear el archivo temporal:

```Elm
import Temporary exposing (file)

myTempFile = 
  Temporary.file "myTempFile.txt" -- nombre del archivo temporal
  (Temporary.WithContents "Hola mundo") -- contenido del archivo temporal
```

## Profundizando

La función `Temporary.file` toma dos argumentos: un *nombre* para el archivo temporal y una *acción* que especifica qué hacer con el archivo. En el ejemplo anterior, utilizamos la acción `Temporary.WithContents` para especificar que el contenido del archivo sea "Hola mundo". Sin embargo, también se pueden utilizar otras acciones, como `Temporary.Empty` para crear un archivo vacío o `Temporary.FromFile` para crear un archivo a partir de otro archivo.

Además, existe la opción de especificar un *directorio* en el cual se creará el archivo temporal utilizando la función `Temporary.fileInDirectory`. Esto puede ser útil cuando se desea tener mayor control sobre la ubicación del archivo temporal.

## Ver también

- [Documentación de la biblioteca de Elm Temporary](https://package.elm-lang.org/packages/justgook/temporary/latest/)
- [Tutorial de programación funcional con Elm](https://www.freecodecamp.org/news/learn-functional-elm-in-10-minutes-fc3aad6a2269/)
- [Ejemplo práctico de creación de archivos temporales en Elm](https://github.com/elm/compiler/blob/master/Temporary.elm)