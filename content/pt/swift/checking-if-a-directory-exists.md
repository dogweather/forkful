---
title:    "Swift: Verificando se um diretório existe"
keywords: ["Swift"]
---

{{< edit_this_page >}}

## Por que verificar se um diretório existe em Swift?

Ao trabalhar com programação, pode ser necessário verificar a existência de um diretório em um determinado sistema de arquivos. Isso pode ser útil para garantir que o seu código funcione corretamente ou para evitar erros ao acessar um diretório inexistente. Neste artigo, vamos explorar como realizar essa verificação em Swift.

## Como fazer

Para verificar se um diretório existe em Swift, podemos utilizar o método `FileManager.default.fileExists(atPath:)`, que retorna um valor booleano indicando se o diretório existe ou não. Veja um exemplo de código abaixo:

```swift
let directoryPath = "/Users/username/Documents"
if FileManager.default.fileExists(atPath: directoryPath) {
    print("O diretório existe!")
} else {
    print("O diretório não existe.")
}
```

No exemplo acima, estamos verificando se o diretório `Documents` existe no sistema de arquivos do usuário atual. Se o diretório existir, a mensagem "O diretório existe!" será impressa no console, caso contrário, a mensagem "O diretório não existe." será exibida.

## Mergulho profundo

Por trás do método `fileExists(atPath:)`, temos o `FileManager`, uma classe que fornece uma interface para trabalhar com os arquivos e diretórios do sistema de arquivos. O método em questão utiliza o caminho fornecido como parâmetro para verificar se existe algum arquivo ou diretório com essa URL. Isso significa que podemos fornecer não apenas o caminho absoluto como no exemplo acima, mas também uma URL relativa a partir de um diretório específico.

Além disso, é importante mencionar que o método `fileExists(atPath:)` não verifica se o caminho fornecido é um diretório ou um arquivo, apenas retorna `true` se existe uma entrada com esse caminho no sistema de arquivos.

## Veja também

- Documentação oficial da Apple para a classe `FileManager`: https://developer.apple.com/documentation/foundation/filemanager
- Exemplo de código no GitHub mostrando como verificar se um diretório existe em Swift: https://github.com/rodrigoalvesvieira/check-if-directory-exists-swift/blob/master/main.swift