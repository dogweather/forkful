---
title:                "Escrevendo um arquivo de texto"
html_title:           "Swift: Escrevendo um arquivo de texto"
simple_title:         "Escrevendo um arquivo de texto"
programming_language: "Swift"
category:             "Swift"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/swift/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Por que

Escrever um arquivo de texto é uma das habilidades básicas de programação em Swift. Ele permite que você armazene informações e dados de forma organizada e acesse esses dados posteriormente. Isso é especialmente útil para armazenar configurações, dados de usuário e qualquer outro tipo de informação que você queira manter em seu aplicativo.

## Como Fazer

Para escrever um arquivo de texto em Swift, você precisará seguir três etapas básicas: criar o arquivo, escrever os dados desejados e salvar o arquivo.

Comece criando uma variável com um caminho de arquivo para onde deseja salvar o arquivo. Em seguida, você pode usar a função `String()` para converter os dados que deseja escrever em uma string. Por fim, utilize o método `write(toFile:, atomically:)` para salvar a string no arquivo.

Veja um exemplo de código abaixo:

```
var filePath = "caminho/do/arquivo"
var data = String("Este é um exemplo de dados que serão escritos no arquivo")
data.write(toFile: filePath, atomically: true)
```

Ao executar o código acima, você criará um arquivo chamado "arquivo" no caminho especificado e o conteúdo "Este é um exemplo de dados que serão escritos no arquivo" será salvo nele.

## Profundidade

Existem algumas coisas importantes a serem consideradas ao escrever um arquivo de texto em Swift. Uma delas é o fato de que a função `write(toFile:, atomically:)` possui um segundo parâmetro chamado ´atomically´. Se estiver definido como `true`, o sistema garantirá que o arquivo seja escrito de forma atômica, ou seja, o arquivo só será salvo se foram escritos todos os dados com sucesso. Se estiver definido como `false`, o arquivo será salvo imediatamente sem garantia de sucesso.

Outro fator importante é o uso de diferentes caracteres especiais no texto que será escrito. Certifique-se de que esses caracteres estejam codificados corretamente para evitar problemas na exibição do texto posteriormente.

## Veja Também

- Documentação oficial da Apple sobre o método `write(toFile:, atomically:)`:  https://developer.apple.com/documentation/foundation/nsstring/1410790-writetofile
- Exemplos práticos de escrita de arquivos em Swift: https://github.com/AndrewKhrystyan/ios-core-data-documents/tree/master/ios-core-data-basic-level/ios-core-data-basic-level/ios-core-data/documents-db
- Outros artigos sobre programação em Swift: https://www.hackingwithswift.com/