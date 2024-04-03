---
date: 2024-01-26 00:57:35.390988-07:00
description: 'Como fazer: Swift utiliza o tratamento de erros com os blocos `do`,
  `try` e `catch`. Vamos dar uma olhada.'
lastmod: '2024-03-13T22:44:46.927683-06:00'
model: gpt-4-1106-preview
summary: Swift utiliza o tratamento de erros com os blocos `do`, `try` e `catch`.
title: Tratamento de erros
weight: 16
---

## Como fazer:
Swift utiliza o tratamento de erros com os blocos `do`, `try` e `catch`. Vamos dar uma olhada:

```Swift
enum FileError: Error {
    case fileDoesNotExist
    case noPermission
}

func readFile(atPath path: String) throws -> String {
    // Imagine que temos alguma lógica aqui para verificar se um arquivo existe e se temos permissão para lê-lo
    let fileExists = false
    let havePermission = true

    if !fileExists {
        throw FileError.fileDoesNotExist
    }

    if !havePermission {
        throw FileError.noPermission
    }

    return "O conteúdo do arquivo vai aqui"
}

do {
    let fileContent = try readFile(atPath: "/caminho/para/o/arquivo")
    print(fileContent)
} catch FileError.fileDoesNotExist {
    print("Ops! Arquivo não encontrado.")
} catch FileError.noPermission {
    print("Ah! Sem permissão para ler o arquivo.")
} catch {
    print("Ocorreu um erro desconhecido.")
}

```

Saída de Exemplo:

```
Ops! Arquivo não encontrado.
```

## Mergulho Profundo
O tratamento de erros nem sempre foi tão bacana como é agora. Em Objective-C, você lidava com ponteiros para objetos NSError, o que parecia desajeitado. Agora, temos um sistema mais elegante com enums do Swift e o protocolo `Error`.

A instrução `throw` do Swift nos permite sinalizar que algo saiu errado. Blocos `do` agem como domínios conscientes de erros, a prefixação `try` chama o negócio arriscado, e `catch` lida com as coisas se elas derem errado.

Optionals são uma alternativa para situações que não estão exatamente no status de "erro", mas ainda podem ter "nenhum resultado". Eles são um pouco como as variáveis de Schrödinger—ou têm um valor ou não têm.

Para uma profundidade real, confira os tipos `Result`, que são híbridos elegantes entre padrões de retorno regular e de erro.

## Veja Também
- Guia Oficial de Tratamento de Erros do Swift: [Apple Docs](https://docs.swift.org/swift-book/LanguageGuide/ErrorHandling.html)
- Melhores Práticas de Tratamento de Erros em Swift: [RayWenderlich.com](https://www.raywenderlich.com/1851-beginning-swift-error-handling)
- Tratamento Avançado de Erros em Swift: [Artigo do Medium](https://medium.com/better-programming/advanced-error-handling-in-swift-4f6bdf6b01d8)
