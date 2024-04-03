---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:18:12.385484-07:00
description: "Las expresiones regulares, o regex, son secuencias de caracteres que\
  \ forman un patr\xF3n de b\xFAsqueda, a menudo utilizadas para tareas de coincidencia\
  \ o\u2026"
lastmod: '2024-03-13T22:44:59.406856-06:00'
model: gpt-4-0125-preview
summary: "Las expresiones regulares, o regex, son secuencias de caracteres que forman\
  \ un patr\xF3n de b\xFAsqueda, a menudo utilizadas para tareas de coincidencia o\
  \ manipulaci\xF3n de cadenas."
title: Usando expresiones regulares
weight: 11
---

## Cómo:
El soporte nativo de Swift para regex utiliza la clase `NSRegularExpression`, junto con los métodos de rango y reemplazo de la clase String. A continuación, se muestra un ejemplo de uso de regex para encontrar y resaltar direcciones de correo electrónico dentro de un bloque de texto:

```swift
import Foundation

let text = "Contact us at support@example.com or feedback@example.org for more information."
let regexPattern = "[A-Z0-9a-z._%+-]+@[A-Za-z0-9.-]+\\.[A-Za-z]{2,}"

do {
    let regex = try NSRegularExpression(pattern: regexPattern)
    let matches = regex.matches(in: text, range: NSRange(text.startIndex..., in: text))

    if !matches.isEmpty {
        for match in matches {
            let range = Range(match.range, in: text)!
            print("Encontrado: \(text[range])")
        }
    } else {
        print("No se encontraron coincidencias.")
    }
} catch {
    print("Error de Regex: \(error.localizedDescription)")
}

// Salida de muestra:
// Encontrado: support@example.com
// Encontrado: feedback@example.org
```

Para escenarios más complejos o enfocados en la conveniencia, puedes usar bibliotecas de terceros como SwiftRegex, que simplifica la sintaxis y expande las posibilidades. Aunque la biblioteca estándar de Swift es poderosa, algunos desarrolladores prefieren estas bibliotecas por su sintaxis concisa y características adicionales. Así es como podrías realizar una tarea similar usando una biblioteca de terceros hipotética:

```swift
// Asumiendo que existe una biblioteca llamada SwiftRegex y está importada
let text = "Reach out at hello@world.com or visit our website."
let emailPattern = "[A-Z0-9a-z._%+-]+@[A-Za-z0-9.-]+\\.[A-Za-z]{2,}"

let emails = text.matches(for: emailPattern) // Método hipotético proporcionado por SwiftRegex
if emails.isEmpty {
    print("No se encontraron direcciones de correo electrónico.")
} else {
    emails.forEach { email in
        print("Encontrado: \(email)")
    }
}

// Salida hipotética asumiendo que el método `matches(for:)` existe en SwiftRegex:
// Encontrado: hello@world.com
```

Este ejemplo ilustra el uso de un paquete de expresiones regulares de terceros para simplificar la búsqueda de coincidencias dentro de una cadena, asumiendo que existen métodos de conveniencia como `matches(for:)`. Es importante referirse a la documentación de la respectiva biblioteca de terceros para una sintaxis y disponibilidad de métodos precisa.
