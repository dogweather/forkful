---
title:    "Swift: Criando um arquivo temporário"
keywords: ["Swift"]
---

{{< edit_this_page >}}

## Por que criar arquivos temporários em Swift?

Criar arquivos temporários pode ser uma tarefa bastante útil em projetos de programação, especialmente em Swift. Os arquivos temporários são úteis para armazenar pequenas quantidades de dados que são necessários apenas temporariamente. Isso pode ajudar a economizar espaço de armazenamento em seu dispositivo e melhorar a eficiência de seu código.

## Como criar um arquivo temporário em Swift?

 Para criar um arquivo temporário em Swift, você pode utilizar a função `NSTemporaryDirectory()` que retornará o diretório temporário do usuário. Em seguida, você pode especificar um nome para seu arquivo temporário e, finalmente, utilizando a função `FileManager.createFile()` você pode efetivamente criar o arquivo. Veja um exemplo abaixo:

```
let temporaryDir = NSTemporaryDirectory()
let fileName = "tempFile.txt"
let filePath = temporaryDir + fileName

FileManager.createFile(atPath: filePath, contents: nil, attributes: nil)
```

Isso criará um arquivo temporário chamado "tempFile.txt" no diretório temporário do usuário.

## Aprofundando-se na criação de arquivos temporários

Além de simplesmente criar um arquivo temporário, é importante ter em mente que esses arquivos precisam ser apagados após o uso para evitar o acúmulo de arquivos desnecessários em seu dispositivo. Para fazer isso, você pode utilizar a função `FileManager.removeItem(atPath:)` passando o caminho do arquivo temporário como parâmetro.

Além disso, é importante lembrar que a criação de arquivos temporários pode ser útil em diversas situações, como salvar caches de dados ou armazenar informações temporárias durante o processo de execução de um aplicativo.

## Veja também

- [Documentação oficial da Apple sobre a criação de arquivos temporários](https://developer.apple.com/documentation/foundation/filemanager/1407204-createfile) 
- [Tutorial em português sobre a manipulação de arquivos em Swift](https://medium.com/swift-programming/criando-excluindo-e-renomeando-arquivos-em-swift-d0f5e20f4172) 
- [Vídeo tutorial sobre a criação de arquivos temporários em Swift](https://www.youtube.com/watch?v=l_irZSn9aBI)