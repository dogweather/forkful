---
title:                "Encontrando la longitud de una cadena"
html_title:           "Arduino: Encontrando la longitud de una cadena"
simple_title:         "Encontrando la longitud de una cadena"
programming_language: "Swift"
category:             "Swift"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/swift/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## ¿Qué & Por Qué?
Encontrar la longitud de una cadena (string), es el proceso de contar cuántos caracteres hay en una cadena. Los programadores hacen esto para limitar la entrada del usuario, dividir cadenas, validar datos, entre otros.

## ¿Cómo hacerlo?
Aquí hay un ejemplo de cómo usar Swift para encontrar la longitud de una cadena:
```Swift
let cadena = "Hola, mundo"
print("Longitud de la cadena es \(cadena.count)")
```
Este código imprimirá:
```
Longitud de la cadena es 11
```

## Vista Detallada
Para encontrar la longitud de una cadena en Swift, usamos la propiedad `count`. En contraste con otros lenguajes de programación que usan una función `length()`, Swift opta por la propiedad `count` para mantenerse consistente con sus colecciones y arreglos.

Otra forma de hacer esto es utilizando la función `distance(from:to:)` de la propiedad `indices`. Aquí está ese código:
```Swift
let cadena = "Hola, mundo"
let longitud = cadena.distance(from: cadena.startIndex, to: cadena.endIndex)
print("La longitud de la cadena es \(longitud)")
```

Este código también imprimirá:
```
La longitud de la cadena es 11
```
Pero esta alternativa es un tanto más compleja y utilizada principalmente cuando se requiere gran detalle al trabajar con índices personalizados o con diferentes idiomas y conjuntos de caracteres.

## Ver También
1. [Swift String and Characters](https://docs.swift.org/swift-book/LanguageGuide/StringsAndCharacters.html): Documentación oficial de Swift sobre cadenas y caracteres.
2. [¿Qué es una cadena?](https://www.computerhope.com/jargon/s/string.htm): Definición simple y detallada de una cadena.
3. [String Manipulation in Swift](https://www.hackingwithswift.com/articles/141/8-examples-of-strings-in-Swift): Varias formas de manipular cadenas en Swift.