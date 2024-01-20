---
title:                "Criando um arquivo temporário"
html_title:           "Bash: Criando um arquivo temporário"
simple_title:         "Criando um arquivo temporário"
programming_language: "Swift"
category:             "Swift"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/swift/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

# Criando um arquivo temporário em Swift

## O que e Por que?
Os arquivos temporários oferecem um espaço seguro para armazenar e manipular dados que não precisam ser mantidos indefinidamente. Geralmente são usados para armazenar dados de operações grandes ou complexas, onde manter tudo na memória não seria eficiente.

## Como fazer:
Aqui está o código de exemplo em Swift para criar um arquivo temporário:

```Swift
import Foundation

func criarArquivoTemporario() throws -> URL {
    let temporaryDirectoryURL = URL(fileURLWithPath: NSTemporaryDirectory(), isDirectory: true)
    let temporaryFilename = ProcessInfo().globallyUniqueString
    let temporaryFileURL = temporaryDirectoryURL.appendingPathComponent(temporaryFilename)

    try "Hello, Swift".write(to: temporaryFileURL, atomically: true, encoding: .utf8)
    return temporaryFileURL
}

do {
    let temporaryFileURL = try criarArquivoTemporario()
    print("Arquivo temporário criado: \(temporaryFileURL)")
} catch {
    print("Erro ao criar arquivo temporario: \(error)")
}
```
Quando você executar esse código, será impresso o caminho completo do arquivo temporário que contém a string "Hello, Swift".

## Mergulho Profundo
Historicamente, a criação de arquivos temporários era feita usando funções de sistema de baixo nível, mas as APIs modernas de alto nível tornaram isso muito mais fácil e seguro.

Alternativas à criação de arquivos temporários incluem o uso de banco de dados em memória (como SQLite com :memory:) ou tipos de dados em memória (como Array ou Dictionary), mas isso pode rapidamente consumir muita memória para operações realmente grandes.

Quando você cria um arquivo temporário desta maneira, o arquivo físico é adicionado ao sistema imediatamente, mas o arquivo em si não é apagado automaticamente. Você deve gerenciar a exclusão de arquivos temporários que você cria.

## Veja também:
1. <a href="https://developer.apple.com/documentation/foundation/" target="_blank"> Documentação oficial da Fundação Swift na Apple Developer </a>
2. <a href="https://stackoverflow.com" target="_blank"> Stack Overflow para dúvidas gerais sobre programação Swift </a>.