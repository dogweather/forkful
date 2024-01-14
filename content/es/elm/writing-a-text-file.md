---
title:    "Elm: Redactando un archivo de texto"
keywords: ["Elm"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/es/elm/writing-a-text-file.md"
---

{{< edit_this_page >}}

## ¿Por qué escribir un archivo de texto en Elm?

Escribir un archivo de texto en Elm puede ser útil para guardar datos o configuraciones que se puedan utilizar en tu programa. También es una buena forma de aprender a utilizar funciones de entrada/salida en Elm.

## Cómo hacerlo

Para escribir un archivo de texto en Elm, se utiliza la función `File.writeString`.

```
Elm
  File.writeString "miArchivo.txt" "Este es un ejemplo de texto escrito en un archivo."
```

Este código creará un archivo llamado "miArchivo.txt" y escribirá el texto dentro del archivo. Al ejecutar el programa, el archivo se creará en la misma carpeta que el archivo Elm.

## Profundizando

Para obtener más control sobre cómo se escribe el archivo, se puede utilizar la función `File.new` para crear una instancia de `File`, que permite escribir en el archivo usando el método `File.write`.

```
Elm
  file = File.new "miArchivo.txt"

  File.write file "Este es otro ejemplo de texto escrito en un archivo."
```

Además, se puede utilizar la función `File.append` para añadir texto al final del archivo en lugar de sobrescribirlo completamente.

## Ver También

- [Guía de Elm para lectura y escritura de archivos](https://guide.elm-lang.org/io/files.html)
- [Documentación de Elm para el módulo File](https://package.elm-lang.org/packages/elm/file/latest/File)