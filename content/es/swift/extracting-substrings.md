---
title:                "Extrayendo subcadenas"
html_title:           "C: Extrayendo subcadenas"
simple_title:         "Extrayendo subcadenas"
programming_language: "Swift"
category:             "Swift"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/swift/extracting-substrings.md"
---

{{< edit_this_page >}}

## ¿Qué y por qué?

Extracción de subcadenas es sacar partes específicas de una cadena de texto completa en Swift. Los programadores lo hacen para analizar y manipular datos, por ejemplo, ir a través de un URL y extraer solo el dominio.

## ¿Cómo hacerlo?

A continuación te presentaré algunos ejemplos de cómo extraer subcadenas en Swift:

```Swift
let texto = "Hola, Mundo Swift"
let índice = texto.index(texto.startIndex, offsetBy: 5)
let subcadena1 = texto[..<índice]

print(subcadena1)  // se imprime "Hola,"
```

En este ejemplo, asumimos que la cadena de texto original es "Hola, Mundo Swift". Extraemos la subcadena "Hola," usando el indice que se encuentra a 5 caracteres desde el comienzo de la cadena.

```Swift
let rango = texto.index(texto.startIndex, offsetBy: 6)..<texto.index(texto.startIndex, offsetBy: 11)
let subcadena2 = texto[rango]

print(subcadena2)  // se imprime "Mundo"
```

Aquí, creamos un rango para extraer la subcadena "Mundo" que se encuentra entre los índices 6 y 11 en la cadena original.

## Inmersión profunda

La implementación de extracción de subcadenas en Swift es muy eficiente, ya que comparte almacenamiento con la cadena original. Esto significa que extraer subcadenas no es costoso en cuanto a consumo de memoria.

Una alternativa a la extracción de subcadenas puede ser el uso de métodos incorporados en Swift, como "prefix" y "suffix", que permiten obtener los primeros o los últimos n caracteres de una cadena respectivamente.

Historicamente, la extracción de subcadenas no siempre ha sido una tarea sencilla en los lenguajes de programación, pero Swift ha simplificado enormemente este proceso con su enfoque orientado a la seguridad y la claridad del código.

## Ver también

Para profundizar en la extracción de subcadenas y los rangos en Swift, los siguientes recursos son ideales:

1. [Substring - Swift Standard Library | Apple Developer Documentation](https://developer.apple.com/documentation/swift/substring)
2. [String - Swift Standard Library | Apple Developer Documentation](https://developer.apple.com/documentation/swift/string)
3. [Safer Swift Code With Guards](https://www.crashcoding.com/blog/2021/safer-swift-code-with-guards)