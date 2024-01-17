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

# O que e por que?
Verificar se um diretório existe é um processo essencial quando se trabalha com arquivos e pastas em um programa Swift. Isso permite que os programadores verifiquem se um determinado diretório está disponível antes de realizar operações nele, evitando assim possíveis erros e garantindo a integridade dos dados.

# Como fazer:
Verificar se um diretório existe em Swift é muito simples. Basta utilizar o método `fileExists` da classe `FileManager` passando o caminho completo do diretório como parâmetro. O método irá retornar um valor booleano indicando se o diretório existe ou não.

```Swift
if FileManager.default.fileExists(atPath: "caminho/do/diretorio") {
    print("O diretório existe!")
} else {
    print("O diretório não existe!")
}
```

# Mergulho profundo:
Antes do Swift 2.0, a verificação de diretórios era feita utilizando o método `fileExistsAtPath` da classe `NSFileManager`, porém ele foi substituído pela classe `FileManager` na versão mais recente da linguagem.

Uma alternativa para verificar a existência de um diretório é utilizar o método `contentsOfDirectory` da classe `FileManager` para obter a lista de todos os arquivos e pastas presentes no diretório pai e então verificar se o diretório que desejamos existe nessa lista.

É importante lembrar que a verificação de existência de diretórios não garante a segurança na manipulação de arquivos, portanto é importante utilizar outros métodos de validação para garantir a integridade dos dados.

# Veja também:
- Documentação oficial da classe `FileManager`: https://developer.apple.com/documentation/foundation/filemanager
- Tutorial sobre manipulação de arquivos com Swift: https://www.ralbu.com/swift-persistencia-em-arquivos-leitura-e-gravacao