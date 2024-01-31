---
title:                "Trabalhando com YAML"
date:                  2024-01-19
simple_title:         "Trabalhando com YAML"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/swift/working-with-yaml.md"
---

{{< edit_this_page >}}

## O Que é & Porquê?

O YAML é um formato de serialização de dados, fácil de ler e escrever, usado para a configuração de projetos e transmissão de dados entre linguagens. Programadores usam YAML por causa da sua legibilidade e simplicidade na integração com várias tecnologias.

## Como Fazer:

Swift não possui suporte inato para YAML, então vamos usar a biblioteca Yams. Primeiro, instale adicionando `Yams` ao seu arquivo `Package.swift`.

```swift
// swift-tools-version:5.5
import PackageDescription

let package = Package(
    name: "YourProject",
    dependencies: [
        .package(url: "https://github.com/jpsim/Yams.git", from: "4.0.0"),
    ],
    targets: [
        .target(name: "YourProject", dependencies: ["Yams"]),
    ]
)
```

Agora, você pode ler e escrever YAML:

```swift
import Yams

let yamlString = """
name: João
age: 34
languages:
  - Swift
  - Python
"""

do {
    let data = try Yams.load(yaml: yamlString) as? [String: Any]
    print(data?["name"] ?? "Nome desconhecido")
} catch {
    print("Erro ao processar o YAML: \(error)")
}

// Saída esperada:
// João
```

Para escrever em YAML:

```swift
let info = [
    "name": "Maria",
    "age": 29,
    "languages": ["Java", "Kotlin"]
]

do {
    let yamlOutput = try Yams.dump(object: info)
    print(yamlOutput)
} catch {
    print("Erro ao escrever o YAML: \(error)")
}

// Saída esperada:
// name: Maria
// age: 29
// languages:
//   - Java
//   - Kotlin
```

## Mergulho Profundo

YAML, que significa "YAML Ain't Markup Language", surgiu em 2001 como um superconjunto do JSON, oferecendo uma sintaxe mais amigável ao humano. Na Swift, o suporte a YAML é fornecido por bibliotecas de terceiros, como Yams. Alternativas incluem JSON e XML, mas YAML é frequentemente preferido para arquivos de configuração devido à sua legibilidade. Internamente, bibliotecas como Yams convertem texto YAML em tipos Swift nativos.

## Veja Também:

- A biblioteca Yams no GitHub: [Yams](https://github.com/jpsim/Yams)
- Especificação oficial do YAML: [YAML Spec](https://yaml.org/spec/1.2/spec.html)
