---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:18:18.952259-07:00
description: "Como: O suporte nativo do Swift para regex utiliza a classe `NSRegularExpression`,\
  \ juntamente com os m\xE9todos de alcance e substitui\xE7\xE3o da classe String.\u2026"
lastmod: '2024-03-13T22:44:46.911029-06:00'
model: gpt-4-0125-preview
summary: "O suporte nativo do Swift para regex utiliza a classe `NSRegularExpression`,\
  \ juntamente com os m\xE9todos de alcance e substitui\xE7\xE3o da classe String."
title: "Usando express\xF5es regulares"
weight: 11
---

## Como:
O suporte nativo do Swift para regex utiliza a classe `NSRegularExpression`, juntamente com os métodos de alcance e substituição da classe String. Abaixo está um exemplo do uso de regex para encontrar e destacar endereços de email dentro de um bloco de texto:

```swift
import Foundation

let text = "Contacte-nos em support@example.com ou em feedback@example.org para mais informações."
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
        print("Nenhum resultado encontrado.")
    }
} catch {
    print("Erro de Regex: \(error.localizedDescription)")
}

// Saída de amostra:
// Encontrado: support@example.com
// Encontrado: feedback@example.org
```

Para cenários mais complexos ou focados em conveniência, você pode usar bibliotecas de terceiros, como SwiftRegex, que simplifica a sintaxe e expande as possibilidades. Embora a biblioteca padrão do Swift seja poderosa, alguns desenvolvedores preferem estas bibliotecas por sua sintaxe concisa e funcionalidades adicionais. Veja como você poderia realizar uma tarefa semelhante usando uma biblioteca de terceiros hipotética:

```swift
// Assumindo que uma biblioteca chamada SwiftRegex existe e foi importada
let text = "Entre em contato em hello@world.com ou visite nosso site."
let emailPattern = "[A-Z0-9a-z._%+-]+@[A-Za-z0-9.-]+\\.[A-Za-z]{2,}"

let emails = text.matches(for: emailPattern) // Método hipotético fornecido pelo SwiftRegex
if emails.isEmpty {
    print("Nenhum endereço de email encontrado.")
} else {
    emails.forEach { email in
        print("Encontrado: \(email)")
    }
}

// Saída hipotética assumindo que o método `matches(for:)` exista no SwiftRegex:
// Encontrado: hello@world.com
```

Este exemplo ilustra o uso de um pacote de expressão regular de terceiros para simplificar a busca de correspondências dentro de uma string, assumindo que métodos de conveniência como `matches(for:)` existam. É importante consultar a documentação da biblioteca de terceiros respectiva para obter a sintaxe e disponibilidade de métodos precisos.
