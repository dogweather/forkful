---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:06:32.796928-07:00
description: "C\xF3mo hacerlo: Las estructuras `String` de Swift vienen con un par\
  \ de m\xE9todos integrados para manipular el caso de las cadenas. Aqu\xED hay algunos\
  \ enfoques\u2026"
lastmod: '2024-03-13T22:44:59.400313-06:00'
model: gpt-4-0125-preview
summary: "Las estructuras `String` de Swift vienen con un par de m\xE9todos integrados\
  \ para manipular el caso de las cadenas."
title: Capitalizando una cadena de texto
weight: 2
---

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
