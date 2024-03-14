---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:34:44.498816-07:00
description: "Escrever no erro padr\xE3o (stderr) trata de direcionar as mensagens\
  \ de erro ou diagn\xF3sticos do seu programa para uma sa\xEDda separada, distinta\
  \ da sa\xEDda\u2026"
lastmod: '2024-03-13T22:44:46.937134-06:00'
model: gpt-4-0125-preview
summary: "Escrever no erro padr\xE3o (stderr) trata de direcionar as mensagens de\
  \ erro ou diagn\xF3sticos do seu programa para uma sa\xEDda separada, distinta da\
  \ sa\xEDda\u2026"
title: "Escrevendo para o erro padr\xE3o"
---

{{< edit_this_page >}}

## O Que & Por Quê?

Escrever no erro padrão (stderr) trata de direcionar as mensagens de erro ou diagnósticos do seu programa para uma saída separada, distinta da saída padrão (stdout). Isso é crucial para depuração e registro de erros sem poluir a saída padrão, facilitando o entendimento do estado e problemas do programa tanto para desenvolvedores quanto para usuários.

## Como Fazer:

Em Swift, escrever no erro padrão pode ser feito usando a classe `FileHandle` para acesso direto ao stderr. Aqui está um exemplo simples:

```swift
import Foundation

// Define uma mensagem
let errorMessage = "Ocorreu um erro.\n"

// Converte a mensagem para dados
if let data = errorMessage.data(using: .utf8) {
    // Escreve a mensagem de erro no stderr
    FileHandle.standardError.write(data)
}
```

Saída para stderr (tipicamente vista em um console ou terminal):
```
Ocorreu um erro.
```

Para registro de logs mais complexos ou quando trabalhando com bibliotecas externas, pode-se considerar o uso de uma biblioteca de terceiros como **SwiftLog**. Embora **SwiftLog** não escreva no stderr diretamente de cara, você pode implementar um backend de registro personalizado para alcançar isso. Aqui está um exemplo simplificado de definição de um manipulador de log personalizado que escreve no stderr:

Primeiro, adicione **SwiftLog** às dependências do seu projeto em `Package.swift`:
```swift
// swift-tools-version:5.3

import PackageDescription

let package = Package(
    name: "SeuNomeDoPacote",
    dependencies: [
        .package(url: "https://github.com/apple/swift-log.git", from: "1.0.0"),
    ],
    targets: [
        .target(
            name: "SeuNomeDoAlvo",
            dependencies: [
                .product(name: "Logging", package: "swift-log"),
            ]),
    ]
)
```

Então, implemente um manipulador de log personalizado que escreve no stderr:

```swift
import Logging
import Foundation

struct StderrLogHandler: LogHandler {
    let label: String
    
    var logLevel: Logger.Level = .info
    
    func log(level: Logger.Level, message: Logger.Message, metadata: Logger.Metadata?, source: String, file: String, function: String, line: UInt) {
        let output = "\(message)\n"
        if let data = output.data(using: .utf8) {
            FileHandle.standardError.write(data)
        }
    }
    
    subscript(metadataKey metadataKey: String) -> Logger.Metadata.Value? {
        get { return nil }
        set(newValue) { }
    }
    
    var metadata: Logger.Metadata {
        get { return [:] }
        set(newMetadata) { }
    }
}

// Uso
LoggingSystem.bootstrap(StderrLogHandler.init)
let logger = Logger(label: "com.exemplo.seuapp")

logger.error("Esta é uma mensagem de erro")
```

Saída para stderr:
```
Esta é uma mensagem de erro
```

Este manipulador personalizado permite que você direcione suas mensagens de erro SwiftLog diretamente para o erro padrão, integrando-se perfeitamente com outras mensagens de log que sua aplicação possa gerar.
