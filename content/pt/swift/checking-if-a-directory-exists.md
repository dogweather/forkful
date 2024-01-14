---
title:    "Swift: Verificando se um diretório existe"
keywords: ["Swift"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pt/swift/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Por que verificar se um diretório existe?

Às vezes, precisamos saber se um diretório existe antes de realizarmos determinadas operações em nosso código Swift. Por exemplo, ao tentar salvar um arquivo em um diretório específico, é importante garantir que esse diretório realmente exista antes de tentar salvar o arquivo. Verificar se um diretório existe pode nos ajudar a evitar erros e manter nosso código organizado.

## Como verificar se um diretório existe

A verificação da existência de um diretório pode ser feita usando a classe `FileManager` do Swift. Primeiro, precisamos criar uma instância do `FileManager` usando o comando `let fileManager = FileManager.default`. Em seguida, podemos usar o método `fileExists(atPath:)` passando o caminho do diretório que queremos checar. Por exemplo:

```Swift
let fileManager = FileManager.default
let directoryPath = "/Users/usuario/Documentos/MeuDiretorio"

if fileManager.fileExists(atPath: directoryPath) {
    print("O diretório existe!")
} else {
    print("O diretório não existe!")
}
```

A saída desse código será "O diretório existe!" se o diretório em questão existir ou "O diretório não existe!" caso contrário.

## Mergulho Profundo

O método `fileExists(atPath:)` retorna um valor booleano indicando se o diretório existe ou não. Se quisermos também verificar se o caminho passado se refere a um diretório ou a um arquivo, podemos usar o método `fileAttributes(atPath:)` e verificar o valor da propriedade `FileAttributeKey.type` do dicionário retornado. Por exemplo:

```Swift
let fileManager = FileManager.default
let directoryPath = "/Users/usuario/Documentos/MeuDiretorio"

if let attributes = try? fileManager.attributesOfItem(atPath: directoryPath),
let type = attributes[FileAttributeKey.type] as? FileAttributeType, type == .typeDirectory {
    print("O caminho se refere a um diretório!")
} else {
    print("O caminho não se refere a um diretório!")
}
```

## Veja também

- [Documentação oficial do FileManager](https://developer.apple.com/documentation/foundation/filemanager)
- [Guia de referência do Swift: Classes](https://docs.swift.org/swift-book/LanguageGuide/Classes.html)
- [Tutorial sobre como trabalhar com diretórios e arquivos em Swift](https://www.hackingwithswift.com/example-code/system/how-to-check-for-file-existence-using-filemanager)