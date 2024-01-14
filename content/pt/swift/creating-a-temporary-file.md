---
title:                "Swift: Criando um arquivo temporário"
programming_language: "Swift"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/swift/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Por que criar um arquivo temporário em Swift?

Criar arquivos temporários em Swift pode ser útil em muitas situações, como gerar relatórios, armazenar dados temporários ou criar backups de informações importantes. Além disso, isso pode ajudar a manter a organização do seu código e evitar sobrecarregar o armazenamento do dispositivo.

## Como criar um arquivo temporário em Swift

Para criar um arquivo temporário em Swift, você pode usar a função `NSTemporaryDirectory()` para obter o diretório temporário do dispositivo. Em seguida, você pode usar a função `URL(fileURLWithPath:)` para criar um arquivo nesse diretório. Aqui está um exemplo de código:

```Swift
let tempDir = NSTemporaryDirectory()
let tempFileURL = URL(fileURLWithPath: tempDir).appendingPathComponent("meuArquivo.txt")

do {
    try "Este é o meu arquivo temporário!".write(to: tempFileURL, atomically: true, encoding: .utf8)
} catch {
    print("Erro ao criar o arquivo: \(error)")
}
```

Uma vez que você tenha criado o arquivo temporário, você pode usá-lo normalmente como qualquer outro arquivo em seu aplicativo. No exemplo acima, nós escrevemos uma simples string no arquivo, mas você pode armazenar qualquer tipo de dado que precisar.

## Mergulho mais profundo

Criar um arquivo temporário é uma tarefa relativamente simples em Swift, mas existem algumas considerações importantes a serem lembradas. Por exemplo, certifique-se de excluir o arquivo após utilizá-lo, para evitar acúmulo de arquivos temporários no dispositivo. Além disso, é recomendado utilizar o método `createFile(atPath:contents:attributes:)` ao invés de `write(to:atomically:encoding:)`, para garantir que o arquivo seja criado de forma síncrona.

## Veja também

- [Documentação oficial da Apple sobre criação de arquivos temporários em Swift](https://developer.apple.com/documentation/foundation/filemanagertemppathurl)
- [Artigo sobre gerenciamento de arquivos em Swift](https://www.raywenderlich.com/35-filemanager-class-tutorial-for-ios-how-to-work-with-files-in-swift)
- [Stack Overflow: Como criar um arquivo temporário em Swift](https://stackoverflow.com/questions/24097826/how-do-i-create-a-temporary-file-in-swift)