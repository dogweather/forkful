---
title:                "Extracción de subcadenas"
aliases:
- es/swift/extracting-substrings.md
date:                  2024-01-20T17:46:42.829111-07:00
model:                 gpt-4-1106-preview
simple_title:         "Extracción de subcadenas"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/swift/extracting-substrings.md"
---

{{< edit_this_page >}}

## Qué y Por Qué?
Extraer subcadenas es como recortar una parte de una cadena para usarla en tu código. Los programadores lo hacen para manipular y trabajar con datos de texto de manera más eficaz, como validar entradas, buscar patrones o simplemente mostrar partes específicas al usuario.

## Cómo hacerlo:
```Swift
let frase = "Hola, programadores del mundo!"
let indexInicio = frase.index(frase.startIndex, offsetBy: 7)
let indexFin = frase.index(frase.startIndex, offsetBy: 21)

let subcadena = frase[indexInicio...indexFin] // Extrae "programadores"

print(subcadena) // Muestra "programadores"
```
Ejemplo de salida:
```
programadores
```

## En Profundidad:
Históricamente, Swift ha evolucionado para hacer el manejo de cadenas más seguras y eficientes, causando que los métodos para extraer subcadenas hayan cambiado con el tiempo, haciendo uso intensivo de `Index`. Como alternativa, puedes usar `range(of:)` para encontrar un substring específico:
```Swift
if let rango = frase.range(of: "programadores") {
   let subcadena = frase[rango]
   print(subcadena) // Muestra "programadores"
}
```
El manejo de subcadenas también difiere de otros lenguajes como Python o JavaScript, donde es mucho más directo. En Swift, se usa el tipo `Substring` en lugar de `String` para extraer partes, lo cual es importante para el rendimiento porque `Substring` comparte la memoria con la cadena original.

## Ver También:
- Documentación oficial de Swift sobre Strings y Characters: [Swift String and Characters](https://docs.swift.org/swift-book/LanguageGuide/StringsAndCharacters.html)
- Tutorial de Swift sobre el manejo de cadenas: [Ray Wenderlich's Strings Tutorial](https://www.raywenderlich.com/5539282-swift-string-tutorial-for-beginners)
- Artículo sobre la seguridad y eficiencia de las cadenas en Swift: [Swift.org String Manifesto](https://github.com/apple/swift/blob/main/docs/StringManifesto.md)
