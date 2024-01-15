---
title:                "Verificando se um diretório existe"
html_title:           "Swift: Verificando se um diretório existe"
simple_title:         "Verificando se um diretório existe"
programming_language: "Swift"
category:             "Swift"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/swift/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Por que verificar se um diretório existe?

Verificar a existência de um diretório é uma tarefa comum na programação em Swift. Isso pode ser útil para garantir que um diretório necessário para a execução do código esteja presente, ou para evitar erros na manipulação de arquivos.

## Como verificar se um diretório existe em Swift

Para verificar se um diretório existe, podemos utilizar a função `FileManager.default.fileExists(atPath: String)`, que retorna um booleano indicando se o diretório existe ou não. Veja o exemplo abaixo:

```Swift
let fileManager = FileManager.default
let directoryPath = "/Users/MeuUsuario/meuDiretorio" // Substitua pelo caminho do diretório que deseja verificar
if fileManager.fileExists(atPath: directoryPath) {
    print("O diretório existe")
} else {
    print("O diretório não existe")
}
```

A saída do código acima será "O diretório existe" se o diretório especificado existir e "O diretório não existe" se não existir.

## Aprofundando na verificação de diretórios em Swift

A função `fileExists(atPath: String)` utiliza o caminho absoluto do diretório como parâmetro. No entanto, também é possível verificar se um diretório existe utilizando um objeto `URL` com o método `checkResourceIsReachable()`. Além disso, podemos utilizar `isDirectory` para verificar se o caminho especificado é um diretório ou um arquivo comum.

## Veja também
- [Documentação oficial do Swift: FileManager](https://developer.apple.com/documentation/foundation/filemanager)
- [Tutorial: Trabalhando com diretórios em Swift](https://medium.com/@beshermawy/working-with-directories-in-swift-3-aa5dca601808)