---
title:                "Capitalizando una cadena de texto"
html_title:           "Swift: Capitalizando una cadena de texto"
simple_title:         "Capitalizando una cadena de texto"
programming_language: "Swift"
category:             "Swift"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/swift/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## ¿Qué & Por Qué?
Capitalizar una cadena se refiere a convertir la primera letra de cada palabra en mayúscula. Los programadores lo hacen principalmente para mejorar la legibilidad del texto y mantener la coherencia del formato a lo largo del código.

## ¿Cómo hacerlo?

Aquí le mostramos cómo capitalizar una cadena de texto en Swift.

```Swift
let cadena = "hola mundo"
let cadenaCapitalizada = cadena.capitalized
print(cadenaCapitalizada)
```

Su salida será:

```Swift
Hola Mundo
```

## Inmersión Profunda

- **Contexto Histórico**: Swift tomó una decisión consciente de hacer las funciones de cadena fáciles de usar y accesibles, y la funcionalidad para capitalizar una cadena es una de ellas.
- **Alternativas**: Si sólo quieres capitalizar la primera letra de la cadena, puedes hacer algo como esto:

```Swift
var cadena = "hola mundo"
cadena = cadena.capitalizingFirstLetter()
print(cadena)
```

Tu salida será:

```Swift
Hola mundo
```

- **Detalles de Implementación**: `capitalized` en Swift está implementado utilizando las reglas de Unicode para las operaciones de cambio de caso. Esto significa que funcionará correctamente con cadenas que contienen caracteres no latinos.

## Ver También

Para obtener más información sobre las cadenas en Swift, consulta las siguientes fuentes:

- La guía oficial de Apple sobre cadenas y caracteres en Swift: [https://docs.swift.org/swift-book/LanguageGuide/StringsAndCharacters.html](https://docs.swift.org/swift-book/LanguageGuide/StringsAndCharacters.html)
- Un tutorial útil sobre cómo trabajar con cadenas en Swift: [https://www.hackingwithswift.com/quick-start/understanding-swift/how-to-capitalize-the-first-letter-of-a-string](https://www.hackingwithswift.com/quick-start/understanding-swift/how-to-capitalize-the-first-letter-of-a-string)