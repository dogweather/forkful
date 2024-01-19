---
title:                "Convirtiendo una cadena a minúsculas"
html_title:           "Bash: Convirtiendo una cadena a minúsculas"
simple_title:         "Convirtiendo una cadena a minúsculas"
programming_language: "Swift"
category:             "Swift"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/swift/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## ¿Qué y Por Qué?

Convertir una cadena a minúsculas significa transformar todos los caracteres de mayúsculas a minúsculas en una cadena de texto. Los programadores lo hacen para estándarizar los datos y realizar comparaciones sensibles sin tener que preocuparse por las diferencias de caso.

## ¿Cómo hacerlo?

Aquí está cómo convertir un string a minúsculas en Swift. Usamos el método `lowercased()`. Dentro de ```Swift ...``` bloques de código, se ve así:

```Swift
let cadena = "Hola Mundo!"
let cadenaEnMinusculas = cadena.lowercased()

print(cadenaEnMinusculas)  // imprime "hola mundo!"
```

## Buceo Profundo

En los días de los primeros lenguajes de programación como Assembly o C, los programadores tenían que implementar manualmente la conversión de mayúsculas a minúsculas. Hoy en día, casi todos los lenguajes modernos, incluido Swift, proporcionan este tipo de funcionalidades fuera de la caja.

Una alternativa a `lowercased()` en Swift podría ser recorrer cada caracter del string y transformarlo, pero esto es menos eficiente y recomendable.

No debes preocuparte demasiado por los detalles de la implementación del método `lowercased()` a menos que estés trabajando con enormes cantidades de datos y necesites optimizar el rendimiento. Swift utiliza algoritmos de conversión de letras a minúsculas acorde con Unicode.

## Ver También

- Documentación oficial de Swift sobre Strings: [https://docs.swift.org/swift-book/LanguageGuide/StringsAndCharacters.html](https://docs.swift.org/swift-book/LanguageGuide/StringsAndCharacters.html)
- Discussión en el foro de Swift sobre maneras de convertir strings a minúsculas: [https://forums.swift.org/](https://forums.swift.org/)
- Unicode Case Mapping: [https://www.unicode.org/standard/standard.html](https://www.unicode.org/standard/standard.html)