---
title:                "Elm: Leyendo un archivo de texto"
simple_title:         "Leyendo un archivo de texto"
programming_language: "Elm"
category:             "Elm"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/elm/reading-a-text-file.md"
---

{{< edit_this_page >}}

# Por qué
Leer archivos de texto es una habilidad esencial para cualquier programador de Elm. Te permite acceder y manipular información almacenada en un archivo de forma dinámica, lo cual es útil en muchas situaciones. En esta publicación, te mostraremos cómo hacerlo.

# Cómo
```Elm
import File
import Text

fichero : File.Content String
fichero =
  File.read "mi-archivo.txt"

texto : String
texto =
  case fichero of
    File.Error _ ->
      "No se pudo leer el archivo."

    File.Success contenido ->
      contenido

main =
  Text.fromNormal texto
```
El código de arriba importa los módulos necesarios para leer el archivo de texto y luego utiliza la función `File.read` para obtener su contenido. Si el archivo se lee correctamente, el contenido se almacena en la variable `texto` y se imprime en pantalla utilizando la función `Text.fromNormal`. Si ocurre algún error, se mostrará un mensaje apropiado.

# Deep Dive
La función `File.read` acepta dos argumentos: el nombre del archivo y una función que se encarga de procesar su contenido. Esta función recibe un tipo de datos `File.Content`, que puede ser `File.Error` en caso de algún error al leer el archivo, o `File.Success` en caso de éxito. La función también debe especificar el tipo de datos que se espera leer del archivo, en este caso, `String`.

Además, para asegurar que el archivo se lea correctamente, es importante manejar posibles errores con una expresión `case` como se muestra en el ejemplo anterior. Si se desea leer un archivo con un tipo de datos diferente, basta con cambiar `String` por el tipo de dato apropiado.

# Ver también
- [Documentación oficial de File](https://package.elm-lang.org/packages/elm/file/latest/)
- [Tutorial de Elm para principiantes](https://www.elm-tutorial.org/es/)