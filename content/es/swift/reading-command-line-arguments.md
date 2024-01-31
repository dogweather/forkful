---
title:                "Lectura de argumentos de línea de comandos"
date:                  2024-01-20T17:57:05.564595-07:00
model:                 gpt-4-1106-preview
simple_title:         "Lectura de argumentos de línea de comandos"

category:             "Swift"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/swift/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## ¿Qué y Por Qué?
Leer argumentos de la línea de comandos permite que tus programas de Swift acepten datos de entrada al ejecutarse, dándoles flexibilidad y adaptabilidad. Los programadores utilizan esto para personalizar la ejecución del programa basándose en las necesidades de los usuarios o en diferentes entornos de ejecución.

## Cómo Hacerlo:
Swift ofrece el arreglo `CommandLine.arguments` para acceder a los argumentos de la línea de comandos. Vamos a ver cómo usarlo:

```Swift
// main.swift
for argument in CommandLine.arguments {
    print(argument)
}

// Ejecuta el programa en Terminal con: 
// swift run MiPrograma argumento1 argumento2
// Salida esperada:
// /ruta/al/MiPrograma
// argumento1
// argumento2
```
Recuerda que el primer elemento de `CommandLine.arguments` es siempre la ruta del ejecutable.

## Inmersión Profunda
Históricamente, obtener los argumentos de la línea de comandos era más engorroso en lenguajes de bajo nivel, como C, donde se usaba `int main(int argc, char * argv[])`. Swift simplifica este proceso con `CommandLine.arguments`. 

Alternativas a considerar son las librerías de terceros que ofrecen más funcionalidades, como parseo de opciones y banderas, por ejemplo, `Swift Argument Parser`.

En cuanto a implementación, ten en cuenta que si un usuario introduce un argumento con espacios, este debe ir entre comillas para ser considerado como un solo argumento ("argumento con espacios"). También, Swift no provee una forma nativa de parsear argumentos con banderas (como `-f archivo`); para esto, necesitarías crear tu propio parseador o usar una librería.

## Vea También
Para más detalles sobre los argumentos de la línea de comandos en Swift, echa un vistazo a:

- [Documentación oficial de Swift sobre `CommandLine`](https://developer.apple.com/documentation/swift/commandline)
- [Swift Argument Parser para parseo avanzado](https://github.com/apple/swift-argument-parser)
- [Tutorial sobre cómo crear un CLI en Swift](https://www.raywenderlich.com/511-command-line-programs-on-macos-tutorial)
