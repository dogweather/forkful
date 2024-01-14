---
title:    "Swift: Escrevendo um arquivo de texto"
keywords: ["Swift"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pt/swift/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Por que escrever um arquivo de texto?

A escrita de arquivos de texto é uma habilidade essencial para qualquer programador, pois permite que você armazene e manipule dados de forma persistente. Esses arquivos podem ser usados para armazenar informações importantes, como configurações de aplicativos, registros de atividades e muito mais.

## Como escrever um arquivo de texto em Swift

Escrever um arquivo de texto em Swift é bastante simples. Primeiro, você precisa criar uma instância da classe `FileManager`, que lida com a criação e manipulação de arquivos. Em seguida, use o método `createFile(atPath:contents:attributes:)` para criar o arquivo em um diretório específico.

```Swift
let fileManager = FileManager.default
let text = "Este é um exemplo de texto a ser gravado em um arquivo."
let path = fileManager.currentDirectoryPath + "/meuArquivo.txt"
if !fileManager.fileExists(atPath: path) {
    fileManager.createFile(atPath: path, contents: nil, attributes: nil)
}

do {
    try text.write(toFile: path, atomically: false, encoding: .utf8)
    print("Arquivo de texto criado com sucesso!")
} catch {
    print("Erro ao escrever no arquivo: \(error)")
}
```

O código acima cria um arquivo chamado "meuArquivo.txt" no diretório atual e escreve o texto fornecido dentro dele. Se o arquivo já existir, o conteúdo antigo será substituído pelo novo. Certifique-se de incluir o `.utf8` como o valor do parâmetro `encoding` para garantir que os caracteres especiais sejam exibidos corretamente.

## Aprofundando-se na escrita de arquivos de texto

Além do método `createFile`, a classe `FileManager` fornece outros métodos úteis para manipular arquivos de texto, como `contents(atPath:)` para ler o conteúdo de um arquivo em formato de dados e `removeItem(atPath:)` para excluir um arquivo existente.

Além disso, existem bibliotecas e frameworks de terceiros disponíveis que simplificam ainda mais a escrita de arquivos de texto em Swift, permitindo que você se concentre em outras tarefas importantes.

## Veja também

- [Documentação oficial da Apple sobre gerenciamento de arquivos em Swift](https://developer.apple.com/documentation/foundation/filemanager)
- [GitHub - Biblioteca de terceiros para escrita de arquivos de texto em Swift](https://github.com/Meniny/FileManagerKit)