---
title:                "Haskell: Buscando y reemplazando texto"
programming_language: "Haskell"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/haskell/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Por qué
¿Alguna vez te has encontrado en la situación en la que tienes un gran bloque de texto y necesitas reemplazar una palabra o frase en todo él? Ya sea que estés escribiendo un ensayo, un código de programación o simplemente revisando un documento, reemplazar manualmente puede ser un proceso tedioso y propenso a errores. ¡Afortunadamente, hay una solución más eficiente utilizando Haskell!

## Cómo
La búsqueda y reemplazo de texto en Haskell puede ser fácilmente lograda a través de la función `replace` en el módulo `Data.Text`. Esta función toma tres argumentos: la palabra o frase a reemplazar, la palabra o frase de reemplazo y el texto en el que se realizará la búsqueda y reemplazo. Aquí hay un ejemplo simple:

```Haskell
import Data.Text

texto = "Me encanta programar en Haskell. Es un lenguaje elegante y poderoso."
nuevoTexto = replace "Haskell" "Python" texto

main = putStrLn nuevoTexto
```

En este caso, la salida será: "Me encanta programar en Python. Es un lenguaje elegante y poderoso."

Para realizar una búsqueda y reemplazo en un archivo específico, podemos utilizar las funciones `readFile` y `writeFile` en conjunto con `replace`. Aquí hay un ejemplo:

```Haskell
import Data.Text
import System.IO

main = do
    texto <- readFile "texto.txt"
    let nuevoTexto = replace "programación" "codificación" texto
    writeFile "nuevoTexto.txt" nuevoTexto
    putStrLn "¡Reemplazo de texto completado!"
```

## Deep Dive
Además de la función `replace`, hay otras opciones disponibles en Haskell para realizar búsquedas y reemplazos más complejas. Por ejemplo, si queremos reemplazar solo la primera aparición de una palabra o frase, podemos utilizar la función `replaceFirst`. También podemos utilizar expresiones regulares para realizar búsquedas y reemplazos más avanzados a través de funciones del módulo `Text.Regex`.

## Ver también
- [Documentación oficial de Haskell](https://www.haskell.org/documentation/)
- [Tutorial de búsqueda y reemplazo en Haskell](https://wiki.haskell.org/Regular_expressions)
- [Ejemplos de código en Haskell](https://www.haskell.org/onlinereport/io.html)