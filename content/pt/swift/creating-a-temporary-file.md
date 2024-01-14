---
title:                "Swift: Criando um arquivo temporário"
simple_title:         "Criando um arquivo temporário"
programming_language: "Swift"
category:             "Swift"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/swift/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Porquê

Criar arquivos temporários é uma parte essencial da programação em Swift. Eles permitem que você armazene dados temporariamente durante a execução do seu aplicativo, sem precisar salvar permanentemente no dispositivo. Isso pode ser útil para armazenar dados temporários de forma eficiente ou para evitar sobrecarregar a memória do dispositivo.

## Como fazer

Existem algumas maneiras simples de criar um arquivo temporário em Swift. Uma maneira é usar a função `NSTemporaryDirectory()` para obter um caminho para um diretório temporário. Em seguida, você pode usar esse caminho para criar seu arquivo usando a função `URL(for:in:appropriateFor:create:)` do tipo `FileManager`. Aqui está um exemplo de código que cria um arquivo temporário e escreve dados nele:

```Swift
let tempDirectory = NSTemporaryDirectory()
let tempURL = URL(fileURLWithPath: tempDirectory).appendingPathComponent("data.txt")
let data = "Isso é um arquivo temporário em Swift".data(using: .utf8)
try? data.write(to: tempURL)
```

Você também pode usar o tipo `FileHandle` para criar um arquivo temporário e escrever dados nele. Aqui está um exemplo de código usando `FileHandle`:

```Swift
let tempFile = try? FileHandle(forWritingTo:
URL(fileURLWithPath: NSTemporaryDirectory())
.appendingPathComponent("data.txt"))
let data = "Isso é um arquivo temporário em Swift".data(using: .utf8)
tempFile?.write(data)
tempFile?.closeFile()
```

## Mergulho profundo

A criação de arquivos temporários pode ser ainda mais personalizada e otimizada. Você pode especificar o prefixo e sufixo do nome do arquivo, bem como o diretório onde deseja que o arquivo seja criado. Além disso, você pode usar o tipo `URLResourceValues` para definir algumas propriedades do arquivo, como proteção, tamanho e data de modificação. Isso permite que você tenha mais controle sobre o arquivo temporário que está criando.

É importante lembrar que os arquivos temporários são excluídos automaticamente quando o dispositivo é reiniciado. Portanto, certifique-se de não depender deles como armazenamento permanente de dados e sempre limpe os arquivos temporários criados pelo seu aplicativo depois de usá-los.

## Veja também

- [Apple Documentation - FileManager](https://developer.apple.com/documentation/foundation/filemanager)
- [Apple Documentation - FileHandle](https://developer.apple.com/documentation/foundation/filehandle)
- [Apple Documentation - URLResourceValues](https://developer.apple.com/documentation/foundation/urlresourcevalues)