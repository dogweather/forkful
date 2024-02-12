---
title:                "Capitalizando una cadena de texto"
aliases:
- /es/swift/capitalizing-a-string.md
date:                  2024-02-03T19:06:32.796928-07:00
model:                 gpt-4-0125-preview
simple_title:         "Capitalizando una cadena de texto"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/swift/capitalizing-a-string.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## ¿Qué y Por Qué?

Capitalizar una cadena en Swift modifica la cadena dada de modo que su primer carácter sea mayúscula y los caracteres restantes sean minúsculas. Los programadores hacen esto por propósitos como formatear nombres o frases de acuerdo con las reglas gramaticales o los estándares de la interfaz de usuario.

## Cómo hacerlo:

Las estructuras `String` de Swift vienen con un par de métodos integrados para manipular el caso de las cadenas. Aquí hay algunos enfoques para capitalizar cadenas en Swift, incluido el uso de métodos estándar y bibliotecas de terceros si es necesario.

### Usando métodos integrados

Para capitalizar la primera letra de una cadena y convertir el resto en minúsculas:

```swift
let myString = "hola, mundo"
let capitalizedString = myString.prefix(1).uppercased() + myString.dropFirst().lowercased()
print(capitalizedString) // Salida: "Hola, mundo"
```

Para capitalizar la primera letra de cada palabra en una oración, puedes usar la propiedad `capitalized`:

```swift
let sentence = "hola, mundo"
let capitalizedSentence = sentence.capitalized
print(capitalizedSentence) // Salida: "Hola, Mundo"
```

### Usando una biblioteca de terceros

Aunque la biblioteca estándar de Swift es bastante completa, algunos formatos de capitalización específicos podrían requerir operaciones más complejas o pueden simplificarse utilizando bibliotecas de terceros. Una de las más populares para la manipulación de cadenas es SwiftRichString. (Nota: Siempre asegúrate de incluir bibliotecas de terceros a través de Swift Package Manager, CocoaPods o Carthage, e importarlas en tu archivo.)

Primero, necesitarías agregar `SwiftRichString` a tu proyecto. Una vez instalado, puedes usarlo para realizar varias operaciones de cadena, incluidas necesidades de capitalización específicas. Sin embargo, hasta ahora, los métodos integrados de Swift cubren adecuadamente la mayoría de los casos de uso de capitalización sin necesidad de bibliotecas externas solo para capitalizar cadenas.

Siempre consulta la documentación más reciente de la biblioteca para cualquier actualización o cambio en los métodos.
