---
title:                "Lendo um arquivo de texto"
html_title:           "Bash: Lendo um arquivo de texto"
simple_title:         "Lendo um arquivo de texto"
programming_language: "Swift"
category:             "Swift"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/swift/reading-a-text-file.md"
---

{{< edit_this_page >}}

## O Que & Por Quê?
Ler um arquivo de texto envolve extrair dados de um arquivo de texto armazenado em algum lugar. Programadores fazem isso para manipular e utilizar essas informações de diversas maneiras em suas aplicações.

## Como fazer:
Antes de começar, certifique-se de que o texto que deseja ler está em um arquivo .txt. Aqui está um exemplo de leitura de um arquivo de texto usando Swift:

```Swift
import Foundation

let filePath = "/caminho/para/o/arquivo.txt"

do {
    let content = try String(contentsOfFile:filePath, encoding: String.Encoding.utf8)
    print(content)
} catch {
    // Tratamento de erro
    print("Não foi possível ler o arquivo.")
}
```
Depois de executar o código acima, o conteúdo do seu arquivo de texto será impresso no console.

## Mergulho Profundo

1. **Contexto histórico**: A leitura de arquivos faz parte da programação desde os seus primórdios. Antes, era usada para armazenar dados, programas e configurações. Ainda hoje, é uma maneira eficaz de manejar grandes volumes de dados sem precisar inseri-los manualmente no código.

2. **Alternativas**: Há muitas outras maneiras de ler arquivos em Swift, um exemplo é usar o `URL` em vez do `String`. Isso pode ser útil para ler arquivos que estão na internet, por exemplo.

3. **Detalhes de implementação**: O método ```contentsOfFile``` que estamos usando aqui é um método de alto nível fornecido pela Apple. Ele cuida de muitos detalhes por trás das cenas, como abrir o arquivo, ler seus dados e, em seguida, fechar o arquivo.

## Veja Também
Para mais detalhes sobre a linguagem Swift e leitura de arquivos, confira estes links:

1. [Documentação Oficial Swift](https://docs.swift.org/swift-book/LanguageGuide/StringsAndCharacters.html)
2. [Leitura de arquivos com Swift por Ray Wenderlich](https://www.raywenderlich.com/418-working-with-the-filemanager-in-swift)