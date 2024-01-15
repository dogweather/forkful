---
title:                "Leyendo un archivo de texto"
html_title:           "Haskell: Leyendo un archivo de texto"
simple_title:         "Leyendo un archivo de texto"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/haskell/reading-a-text-file.md"
---

{{< edit_this_page >}}

## ¿Por qué leer un archivo de texto?

Existen muchas razones por las cuales podrías querer leer un archivo de texto en Haskell. Puede ser que estés trabajando con datos almacenados en un archivo de texto, o que simplemente quieras leer un archivo para mostrar su contenido en pantalla.

Sin importar la razón, ¡la lectura de archivos de texto es una habilidad importante para cualquier programador de Haskell!

## Cómo hacerlo

Para leer un archivo de texto en Haskell, necesitamos usar la función `readFile`. Esta función recibe como argumento la ruta hacia el archivo de texto que queremos leer y devuelve como resultado un valor de tipo `IO String`.

Veamos un ejemplo de cómo leer un archivo llamado "datos.txt" y mostrar su contenido en pantalla:

```Haskell
main = do
  contenido <- readFile "datos.txt"
  putStrLn contenido
```

En este ejemplo, usamos la función `putStrLn` para mostrar el contenido del archivo en la consola. También podríamos hacer cualquier otra operación con el contenido del archivo, como trabajar con sus datos o guardarlo en una variable para su posterior uso.

## Profundizando en la lectura de archivos de texto

La función `readFile` nos devuelve un valor de tipo `IO String`, lo que significa que está empaquetado dentro del tipo `IO`. Esto nos permite tener un control más preciso sobre cómo se ejecuta nuestra acción de lectura de archivos.

Por ejemplo, podemos encadenar la función `readFile` con otras acciones `IO` utilizando la sintaxis del operador `>>=`:

```Haskell
main = do
  contenido <- readFile "datos.txt"
  contenidoEnMayusculas <- (return . toUpper) contenido
  putStrLn contenidoEnMayusculas
```

En este ejemplo, usamos la función `toUpper` para convertir todo el contenido del archivo en mayúsculas antes de mostrarlo en la consola. Nota cómo debemos envolver la función `toUpper` en el operador `return` para poder realizar esta operación dentro de la monada `IO`.

## Ver también

¡Con este artículo ya tienes los conocimientos básicos para leer archivos de texto en Haskell! Pero si quieres aprender más sobre este lenguaje funcional, aquí tienes algunos recursos adicionales:

- [Haskell.org](https://www.haskell.org/) - Página oficial de Haskell, con recursos de aprendizaje y documentación.
- [Real World Haskell](http://book.realworldhaskell.org/read/) - Libro gratuito sobre Haskell.
- [Haskell Exercises](https://exercism.io/tracks/haskell) - Plataforma con ejercicios prácticos para aprender Haskell.