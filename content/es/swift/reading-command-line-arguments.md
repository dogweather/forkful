---
title:    "Swift: Leyendo argumentos de línea de comandos"
keywords: ["Swift"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/es/swift/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Por qué

Aprender a leer argumentos de línea de comando es una habilidad importante en el desarrollo de aplicaciones Swift. Esto te permitirá crear programas más interactivos y personalizables para los usuarios, lo que hará que tus aplicaciones sean más populares y útiles.

## Cómo hacerlo

Para leer los argumentos de línea de comando en Swift, primero debes importar la librería `Foundation`. Luego, usa la variable `CommandLine.arguments` para acceder a los argumentos pasados al programa en tiempo de ejecución. A continuación, puedes usar un bucle para recorrer los argumentos y hacer lo que necesites con ellos.

```Swift
import Foundation

let arguments = CommandLine.arguments // Obtener argumentos
for argument in arguments { // Recorrer argumentos
    print(argument) // Imprimir cada argumento
}
```

Si ejecutas el programa con argumentos, verás que se imprimen en la consola en el orden en que fueron pasados. Por ejemplo, si ejecutas `./miPrograma argumento1 argumento2`, verás que se imprimen `argumento1` y `argumento2`.

## Profundizando

La variable `CommandLine.arguments` es una matriz (array) de tipo `String`, por lo que puedes acceder a los argumentos utilizando índices. También puedes utilizar los métodos de la librería `Foundation` para manipular los argumentos de forma más avanzada, como convertirlos a otros tipos de datos o modificarlos antes de usarlos en tu programa. Además, puedes utilizar la estructura `OptionKit` para manejar argumentos con opciones y parámetros de forma más específica y eficiente.

## Ver también

A continuación, te dejamos algunos enlaces útiles para que sigas aprendiendo sobre cómo leer argumentos de línea de comando en Swift:

- [Documentación oficial de Swift](https://docs.swift.org/swift-book/LanguageGuide/AdvancedOperators.html)
- [Tutorial de AppCoda](https://www.appcoda.com.tw/command-line-argument/)
- [Artículo de Hacking with Swift](https://www.hackingwithswift.com/read/15/5/reading-command-line-inputs)
- [Tutorial de Ray Wenderlich](https://www.raywenderlich.com/7674983-sending-receiving-and-parsing-json-data-with-swiftui)