---
title:                "Criando um arquivo temporário"
html_title:           "Swift: Criando um arquivo temporário"
simple_title:         "Criando um arquivo temporário"
programming_language: "Swift"
category:             "Swift"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/swift/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Por que

Criar arquivos temporários pode ser uma tarefa útil em projetos de desenvolvimento de software. Eles permitem que você armazene dados temporários de maneira eficiente e segura, sem ocupar espaço no disco rígido do seu dispositivo.

## Como Fazer

```Swift
let tempDirectory = URL(fileURLWithPath: NSTemporaryDirectory(), isDirectory: true)
do {
    let tempFile = try FileManager.default.url(for: .itemReplacementDirectory, in: .userDomainMask, appropriateFor: tempDirectory, create: true)
    print("Arquivo temporário criado em \(tempFile)")
} catch {
    print("Não foi possível criar o arquivo temporário: \(error)")
}
```

Saída:

```
Arquivo temporário criado em file:///Users/usuario/Library/Containers/com.example.app/Documents/file.tmp/
```

## Deep Dive

Criar um arquivo temporário envolve alguns conceitos importantes. Primeiramente, é essencial entender os diferentes tipos de diretórios no sistema de arquivos, como o diretório temporário e o diretório de substituição de item (item replacement directory). Além disso, o uso do gerenciador de arquivos (FileManager) e tratamento de erros são fundamentais para garantir que o arquivo temporário seja criado com sucesso.

## Veja Também

- [Documentação oficial do Swift sobre o gerenciador de arquivos](https://developer.apple.com/documentation/foundation/filemanager)
- [WWDC 2015: Working with Files and the File System](https://developer.apple.com/videos/play/wwdc2015/707/)