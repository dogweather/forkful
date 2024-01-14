---
title:    "Swift: Criando um arquivo temporário"
keywords: ["Swift"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pt/swift/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Por que criar um arquivo temporário no Swift?

Criar um arquivo temporário pode ser útil em várias situações de programação. Algumas das razões mais comuns incluem a necessidade de armazenar dados temporários ou de criar um ambiente de teste isolado.

## Como criar um arquivo temporário em Swift

Para criar um arquivo temporário no Swift, podemos usar a classe `FileManager` e o método `FileManager.temporaryDirectory` para obter o diretório temporário do sistema. Em seguida, podemos usar o método `FileManager.temporaryFile()` para criar um arquivo temporário dentro desse diretório.

```Swift
let fileManager = FileManager.default
let tempDir = fileManager.temporaryDirectory
let tempFileURL = tempDir.appendingPathComponent("meuArquivoTemporario.txt")

guard fileManager.createFile(atPath: tempFileURL.path, contents: nil, attributes: nil) else {
    fatalError("Não foi possível criar o arquivo temporário.")
}

print("Arquivo temporário criado em: \(tempFileURL.path)")
```

A saída deste código seria algo como: `Arquivo temporário criado em: /var/folders/3f/n3lwvw7j3kn_hsmds77lfp0h0000gn/T/meuArquivoTemporario.txt`

## Explorando a criação de um arquivo temporário

Criar um arquivo temporário é útil quando precisamos armazenar dados temporários, mas é importante lembrar que esse arquivo será excluído assim que o processo que o criou for encerrado. Além disso, podemos especificar um `prefixo` e um `sufixo` para o nome do arquivo temporário para torná-lo mais identificável.

Uma dica importante é usar o método `FileManager.isDeletableFile(atPath:)` para garantir que podemos excluir o arquivo temporário quando necessário.

## Veja também

Quer aprender mais sobre como trabalhar com arquivos em Swift? Confira os seguintes recursos:

- [Documentação da classe FileManager](https://developer.apple.com/documentation/foundation/filemanager)
- [Tutorial sobre manipulação de arquivos em Swift](https://www.raywenderlich.com/9014-working-with-files-in-swift)