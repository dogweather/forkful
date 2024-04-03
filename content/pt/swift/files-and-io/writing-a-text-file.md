---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:29:35.938895-07:00
description: 'Como fazer: #.'
lastmod: '2024-03-13T22:44:46.939092-06:00'
model: gpt-4-0125-preview
summary: '#.'
title: Escrevendo um arquivo de texto
weight: 24
---

## Como fazer:


### Usando a Biblioteca Padrão do Swift
A biblioteca padrão do Swift inclui todas as ferramentas necessárias para escrever arquivos de texto. Aqui está uma abordagem básica:

```swift
import Foundation

let content = "Olá, leitores do Wired! Aprender Swift é divertido."
let filePath = NSSearchPathForDirectoriesInDomains(.documentDirectory, .userDomainMask, true)[0] as String
let fileName = "\(filePath)/exemplo.txt"

do {
    try content.write(toFile: fileName, atomically: false, encoding: String.Encoding.utf8)
    print("Arquivo escrito com sucesso")
} catch let error as NSError {
    print("Falha ao escrever no URL: \(fileName), Erro: " + error.localizedDescription)
}
```

Este trecho de código escreve uma string em um arquivo chamado `exemplo.txt` no diretório de documentos. Ele lida com possíveis erros usando a manipulação de erros do-try-catch do Swift.

### Usando FileManager para Mais Controle
Para mais controle sobre os atributos do arquivo ou para verificar se o arquivo já existe, `FileManager` pode ser usado:

```swift
import Foundation

let fileManager = FileManager.default
let directories = fileManager.urls(for: .documentDirectory, in: .userDomainMask)
if let documentDirectory = directories.first {
    let fileURL = documentDirectory.appendingPathComponent("exemplo.txt")
    let content = "Explorar Swift para gerenciamento de arquivos é esclarecedor."

    if fileManager.fileExists(atPath: fileURL.path) {
        print("Arquivo já existe")
    } else {
        do {
            try content.write(to: fileURL, atomically: true, encoding: .utf8)
            print("Arquivo criado e escrito com sucesso")
        } catch {
            print("Erro ao escrever arquivo: \(error)")
        }
    }
}
```

### Usando Bibliotecas de Terceiros
Uma biblioteca de terceiros popular para operações de sistema de arquivos em Swift é `Files` por John Sundell:

Primeiro, adicione Files ao seu projeto, geralmente via Swift Package Manager.

```swift
// swift-tools-version:5.3
import PackageDescription

let package = Package(
    name: "SeuNomeDePacote",
    dependencies: [
        .package(url: "https://github.com/JohnSundell/Files", from: "4.0.0"),
    ],
    targets: [
        .target(
            name: "SeuNomeDeAlvo",
            dependencies: ["Files"]),
    ]
)
```

Depois, use-o para escrever em um arquivo:

```swift
import Files

do {
    let file = try File(path: "/caminho/para/seu/diretório/exemplo.txt")
    try file.write(string: "Swift e a biblioteca Files formam uma combinação poderosa.")
    print("Arquivo escrito com sucesso usando a biblioteca Files.")
} catch {
    print("Ocorreu um erro: \(error)")
}
```

Com a biblioteca `Files`, o manuseio de arquivos se torna mais direto, permitindo que você se concentre na lógica de negócios da sua aplicação em vez das minúcias do gerenciamento de arquivos.
