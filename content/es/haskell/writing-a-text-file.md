---
title:                "Haskell: Escribiendo un archivo de texto"
simple_title:         "Escribiendo un archivo de texto"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/haskell/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Por qué

En el mundo de la programación, hay una variedad de herramientas y lenguajes de programación disponibles para escribir diferentes tipos de programas. Uno de estos lenguajes es Haskell, que se caracteriza por su fuerte sistema de tipos y su capacidad de escribir programas muy expresivos y concisos. Una de las tareas básicas de la programación es la escritura de archivos de texto, y en esta publicación te enseñaré cómo hacerlo en Haskell.

## Cómo hacerlo

Para escribir un archivo de texto en Haskell, primero debes importar el módulo `System.IO`, que contiene las funciones necesarias para trabajar con archivos. Luego, puedes utilizar la función `writeFile` para crear un nuevo archivo o sobrescribir un archivo existente. Por ejemplo, para crear un archivo llamado "saludo.txt" con el contenido "¡Hola mundo!", puedes escribir lo siguiente:

```Haskell
import System.IO

main = do
    writeFile "saludo.txt" "¡Hola mundo!"
```

Al ejecutar este programa, se creará un nuevo archivo de texto con el nombre "saludo.txt" en la misma ubicación que el archivo de Haskell. El segundo parámetro de la función `writeFile` es el contenido del archivo, que en este caso es simplemente una cadena de texto.

También puedes utilizar la función `appendFile` para agregar contenido a un archivo existente sin sobrescribirlo. Por ejemplo, si queremos agregar la frase "¡Buen día!" al archivo "saludo.txt", podemos escribir lo siguiente:

```Haskell
import System.IO

main = do
    appendFile "saludo.txt" "¡Buen día!"
```

Esto añadirá la frase al final del archivo existente.

## Profundizando

La función `writeFile` toma dos parámetros: la ruta del archivo y el contenido que queremos escribir en él. Si quieres tener más control sobre cómo se manejan los errores o quieres especificar un formato de archivo específico, puedes utilizar la función `withFile` en su lugar. Esta función toma tres parámetros: la ruta del archivo, el modo de apertura (lectura, escritura, etc.) y una función que se encargará de escribir en el archivo. Esta función recibirá un manejador de archivo, que es una representación del archivo en memoria, y puedes utilizar funciones como `hGetLine` para leer contenido del archivo y `hPutStrLn` para escribir en él.

Otra forma de escribir archivos de texto en Haskell es utilizando la sintaxis de "do" junto con la función `hPutStrLn`. Esta sintaxis permite escribir código más similar a un lenguaje imperativo, lo que puede ser más fácil para aquellos que están acostumbrados a otros lenguajes de programación.

## Ver también

- [Documentación oficial de Haskell](https://www.haskell.org/documentation/)
- [Tutorial de Haskell en español](https://www.twanvl.nl/files/programming/haskell/sf-es.pdf)
- [Artículo sobre manejo de archivos en Haskell](https://wiki.haskell.org/File_manipulation)