---
title:    "Swift: Lendo um arquivo de texto"
keywords: ["Swift"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pt/swift/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Por que ler arquivos de texto em Swift?

Então você decidiu que quer aprender Swift, mas por onde começar? Uma ótima maneira de começar a praticar suas habilidades de programação é lendo e escrevendo em arquivos de texto. Isso pode parecer um conceito simples, mas é essencial para qualquer programador. Ler arquivos de texto em Swift pode ajudá-lo a entender como os dados são armazenados e manipulados, além de ser uma habilidade muito útil em diversos projetos. Continue lendo para aprender como fazer isso!

## Como ler arquivos de texto em Swift

Para ler um arquivo de texto em Swift, você precisará seguir alguns passos simples:

1. Em primeiro lugar, você precisará criar uma instância do `FileManager`, que é a classe responsável por gerenciar arquivos no iOS ou MacOS.
2. Em seguida, utilize o método `urls(for:in:)` para encontrar o caminho do arquivo que você deseja ler.
3. Crie um `InputStream` com o caminho do seu arquivo.
4. Use o método `open()` para abrir o arquivo.
5. Agora que o arquivo foi aberto, você pode ler o conteúdo do arquivo usando o método `read()`.
6. Por fim, lembre-se de fechar o arquivo utilizando o método `close()`.

Seu código deve se parecer com este:

```Swift
let fileManager = FileManager()
let fileURLs = fileManager.urls(for: .documentDirectory, in: .userDomainMask)
let fileURL = fileURLs.appendingPathComponent("meuArquivo.txt")
let inputStream = InputStream(url: fileURL)
inputStream?.open()
var buffer = [UInt8](repeating: 0, count: 1024)
while inputStream!.hasBytesAvailable {
    let bytesRead = inputStream!.read(&buffer, maxLength: buffer.count)
    if bytesRead < 0 { break }
    let data = Data(bytes: &buffer, count: bytesRead)
    print(String(data: data, encoding: .utf8))
}
inputStream?.close()
```

Este código irá ler o conteúdo do arquivo `meuArquivo.txt` e imprimir na tela.

## Aprofunde-se na leitura de arquivos de texto

Existem várias formas de ler arquivos de texto em Swift, como utilizando o método `contentsOfFile` da classe `String` ou utilizando a biblioteca `Foundation` para acessar o conteúdo do arquivo diretamente. Você também pode usar o método `write` para escrever dados em um arquivo de texto.

Além disso, você pode aprender a ler e escrever arquivos de texto separados por vírgula, o que é útil para trabalhar com dados em formato de tabela. Essas são apenas algumas das possibilidades e é importante que você continue praticando para se aprofundar neste assunto e se tornar um expert em leitura de arquivos de texto em Swift.

## Veja também

Aqui estão alguns links úteis para você continuar a sua jornada de aprendizado em Swift:

- [Documentação oficial da Apple sobre FileManager](https://developer.apple.com/documentation/foundation/filemanager)
- [Tutorial sobre leitura de arquivos de texto em Swift](https://www.raywenderlich.com/1148411-reading-and-writing-files-in-swift)
- [Exemplos de código em Swift para ler e escrever arquivos de texto](https://www.hackingwithswift.com/example-code/system/how-to-read-and-write-files-in-swift)