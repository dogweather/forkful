---
title:                "Swift: Leyendo argumentos de línea de comando"
simple_title:         "Leyendo argumentos de línea de comando"
programming_language: "Swift"
category:             "Swift"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/swift/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Por qué

En el mundo de la programación, es importante tener conocimiento sobre cómo utilizar la línea de comando para interactuar con un programa. Una de las tareas más comunes que pueden surgir es la lectura de argumentos de línea de comando. En este artículo, hablaremos sobre por qué es importante aprender a hacerlo y cómo hacerlo en Swift.

## Cómo hacerlo

Para leer argumentos de línea de comando en Swift, necesitamos utilizar la clase `CommandLine` que está incluida en la biblioteca estándar de Swift. Esta clase nos permite acceder a los argumentos de línea de comando como un arreglo de cadenas. Veamos un ejemplo de código:

```Swift
// Este es un ejemplo de un programa que toma dos argumentos de línea de comando y los imprime en la consola

import Foundation

let arguments = CommandLine.arguments

let firstArgument = arguments[1]
let secondArgument = arguments[2]

print("El primer argumento es: \(firstArgument)")
print("El segundo argumento es: \(secondArgument)")
```

Supongamos que corremos este código desde la línea de comando con los argumentos `swift argumentos.swift hola mundo`. La salida sería:

```
El primer argumento es: hola
El segundo argumento es: mundo
```

Como podemos ver, estamos accediendo a los argumentos de línea de comando utilizando el subscript de la clase `CommandLine` y luego los imprimimos en la consola.

## Profundizando

Es importante tener en cuenta que el primer argumento, en este caso `hola`, se encuentra en la posición `1` del arreglo de argumentos, no `0` como podríamos pensar. Esto se debe a que la posición `0` del arreglo contiene el nombre del programa.

Además, es importante mencionar que si necesitamos leer más de dos argumentos de línea de comando, podemos utilizar un loop para acceder a todos ellos.

## Ver también

- [Documentación oficial de Apple sobre la clase CommandLine](https://developer.apple.com/documentation/foundation/commandline)
- [Artículo sobre lectura de argumentos de línea de comando en Swift](https://medium.com/@marianneguenette/swift-command-line-arguments-a11971c421c3)
- [Tutorial detallado sobre el uso de la clase CommandLine en Swift](https://www.appcoda.com/command-line-arguments-swift/)