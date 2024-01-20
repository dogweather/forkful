---
title:                "Leyendo argumentos de la línea de comandos"
html_title:           "Bash: Leyendo argumentos de la línea de comandos"
simple_title:         "Leyendo argumentos de la línea de comandos"
programming_language: "Swift"
category:             "Swift"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/swift/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

# Leyendo argumentos de línea de comandos en Swift

## ¿Qué y por qué?

Los argumentos de línea de comandos son entradas que los programadores brindan al ejecutar sus programas desde la consola. Son útiles porque permiten al usuario influir en cómo se ejecuta el programa sin cambiar el código.

## ¿Cómo hacerlo?

Vamos a ver cómo usar una función simple que imprime los argumentos de la línea de comando en Swift.

```Swift
// main.swift

import Foundation

let argumentos = CommandLine.arguments

print("Número de argumentos: \(argumentos.count)")

for (indice, argumento) in argumentos.enumerated() {
    print("Argumento \(indice): \(argumento)")
}
```

Si ejecuta este programa con `swift main.swift hola mundo`, obtendrá:

```Command Line
Número de argumentos: 3
Argumento 0: main.swift
Argumento 1: hola
Argumento 2: mundo
```

## Inmersión profunda

El manejo de argumentos de línea de comandos ha sido un aspecto central de la programación desde los primeros días del desarrollo de software. En Swift, podemos usar `CommandLine.arguments` para acceder a estos argumentos.

Alternativamente, si necesita más control sobre los argumentos de línea de comandos, puede usar bibliotecas disponibles en Swift, como "Swift-Args" o "CommandLineKit".

`CommandLine.arguments` es una matriz de cadenas, donde el primer elemento siempre es la ruta del programa en sí, y los argumentos de línea de comandos adicionales se añaden a la lista de manera secuencial.

## Ver también

Para profundizar en los argumentos de línea de comandos y cómo usarlos en Swift, consulta los siguientes enlaces:

- [Apple: Basics of Command Line Programming](https://developer.apple.com/library/archive/documentation/Cocoa/Conceptual/ProgrammingWithObjectiveC/WorkingwithBlocks/WorkingwithBlocks.html#//apple_ref/doc/uid/TP40011210-CH8-SW1)
- [GitHub: Swift-Args](https://github.com/apple/swift-args)
- [GitHub: CommandLineKit](https://github.com/objecthub/swift-commandlinekit)