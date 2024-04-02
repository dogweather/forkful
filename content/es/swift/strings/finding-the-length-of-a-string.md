---
date: 2024-01-20 17:48:15.335831-07:00
description: "En Swift, encontrar la longitud de un cadena (String) significa contar\
  \ cu\xE1ntos caracteres contiene. Los programadores lo hacen para validar entradas,\u2026"
lastmod: '2024-03-13T22:44:59.407869-06:00'
model: gpt-4-1106-preview
summary: "En Swift, encontrar la longitud de un cadena (String) significa contar cu\xE1\
  ntos caracteres contiene. Los programadores lo hacen para validar entradas,\u2026"
title: Calculando la longitud de una cadena
weight: 7
---

## What & Why?
En Swift, encontrar la longitud de un cadena (String) significa contar cuántos caracteres contiene. Los programadores lo hacen para validar entradas, limitar texto en UI, o manejar datos correctamente.

## How to:
Swift facilita el conteo de caracteres en una cadena. Usamos la propiedad `count` del String.

```Swift
let saludo = "Hola"
let longitud = saludo.count
print("La cadena '\(saludo)' tiene \(longitud) caracteres.")
```

Salida:
```
La cadena 'Hola' tiene 4 caracteres.
```

Si quieres experimentar con más ejemplos, puedes intentar agregar emojis o caracteres especiales, verás que Swift cuenta los caracteres de manera bastante intuitiva.

```Swift
let cadenaConEmoji = "Programando en Swift 🚀"
print("Caracteres: \(cadenaConEmoji.count)")
```

Salida:
```
Caracteres: 23
```

## Deep Dive
Historicamente, el conteo de caracteres no siempre ha sido directo debido a la complejidad de las codificaciones de caracteres y la representación en la memoria. En los primeros días, cada carácter se mapeaba a un byte, lo que simplificaba las cosas. Hoy en día, Unicode y sus variadas longitudes de caracteres, como los emojis, requieren de una lógica más compleja.

Swift usa Unicode para representar Strings, por lo que `count` devuelve la cantidad correcta de caracteres visibles, sin importar la complejidad del carácter Unicode. No obstante, hay alternativas como `utf8.count`, `utf16.count`, y `unicodeScalars.count`, que contabilizan las unidades de código UTF-8, UTF-16 y escalares Unicode respectivamente, y son útiles en contextos específicos de manejo de bajo nivel.

Por ejemplo, un mismo carácter puede tener diferentes representaciones en Unicode (como la forma normalizada o no), lo que potencialmente puede llevar a confusiones al contar caracteres si no se está atento a la normalización de la cadena.

```Swift
let cafe = "café"
let cafeNormalizado = "cafe\u{0301}"
print(cafe.count)             // 4
print(cafeNormalizado.count)  // 4 aunque la cadena esté normalizada de manera diferente
```

En cuánto a rendimiento, `count` es una operación O(n), donde n es la longitud de la cadena, puesto que Swift necesita iterar a través de todos los caracteres para contarlos.

## See Also
Para una inmersión más profunda en el trabajo con cadenas Unicode en Swift, consulta la documentación oficial de Swift sobre Strings y Characters:

- [Documentación de Apple sobre Strings y Characters](https://docs.swift.org/swift-book/LanguageGuide/StringsAndCharacters.html)
- [Unicode.org's Standard](http://unicode.org/standard/standard.html) para comprender cómo se define y maneja Unicode.
- [Swift String Manifesto](https://github.com/apple/swift/blob/main/docs/StringManifesto.md) para entender la filosofía detrás del diseño de String en Swift.
