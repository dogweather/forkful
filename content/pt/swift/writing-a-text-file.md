---
title:                "Escrevendo um arquivo de texto."
html_title:           "Swift: Escrevendo um arquivo de texto."
simple_title:         "Escrevendo um arquivo de texto."
programming_language: "Swift"
category:             "Swift"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/swift/writing-a-text-file.md"
---

{{< edit_this_page >}}

## O que & Porquê?

Escrever um arquivo de texto é o ato de criar um documento em formato de texto que pode ser lido e editado por humanos, bem como por máquinas. Programadores geralmente escrevem arquivos de texto para armazenar e organizar informações importantes, como configurações, dados de usuários e códigos.

## Como fazer:

Para escrever um arquivo de texto em Swift, você pode seguir os seguintes passos:

1. Importe o framework `Foundation`:
```Swift
import Foundation
```

2. Crie um objeto `URL` que especifica o local onde o arquivo será criado:
```Swift
let fileURL = URL(fileURLWithPath: "caminho/do/arquivo/nome_do_arquivo.txt")
```

3. Crie um objeto `String` que contenha o conteúdo que será escrito no arquivo:
```Swift
let fileContent = "Este é o conteúdo que será escrito no arquivo."
```

4. Use o método `write(to:atomically:encoding:)` do objeto `String` para escrever o conteúdo no arquivo:
```Swift
try? fileContent.write(to: fileURL, atomically: true, encoding: .utf8)
```

5. Pronto! Agora você tem um arquivo de texto criado e preenchido com o conteúdo desejado.

## Profundando mais:

Escrever arquivos de texto é uma tarefa comum na programação, pois é uma forma eficiente de armazenar dados importantes. Embora existam muitas alternativas, como bancos de dados e arquivos binários, os arquivos de texto são simples e universais, podendo ser abertos e alterados facilmente em qualquer plataforma.

Na implementação mostrada acima, usamos o encoding `.utf8`, que é um dos padrões de codificação mais populares para arquivos de texto. Porém, existem outros tipos de codificação, como `isoLatin1` e `ascii`, que podem ser usados dependendo do conteúdo que será escrito.

## Veja também:

- [Documentação oficial da Apple sobre escrita de arquivos](https://developer.apple.com/documentation/foundation/filemanager/1412642-write)
- [Tutorial sobre escrita de arquivos em Swift](https://www.hackingwithswift.com/example-code/system/how-to-write-strings-to-a-text-file-on-disk)