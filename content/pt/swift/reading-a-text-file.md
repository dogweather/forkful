---
title:                "Lendo um arquivo de texto."
html_title:           "Swift: Lendo um arquivo de texto."
simple_title:         "Lendo um arquivo de texto."
programming_language: "Swift"
category:             "Swift"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/swift/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Por que ler um arquivo de texto?

Se você está trabalhando com programação, é muito provável que em algum momento precisará lidar com arquivos de texto. Ler um arquivo de texto pode ser útil para acessar informações armazenadas em um formato legível para humanos, como uma lista de nomes ou endereços.

## Como fazer isso em Swift

Para ler um arquivo de texto em Swift, podemos usar o método `contentsOfFile()` da classe `NSString`. Veja um exemplo de código abaixo:

```Swift
if let path = Bundle.main.path(forResource: "nomes", ofType: "txt") {
  do {
    let nomes = try String(contentsOfFile: path, encoding: .utf8)
    print(nomes)
  } catch {
    print("Erro ao ler o arquivo: \(error)")
  }
}
```

No código acima, usamos o método `path(forResource:ofType:)` para obter o caminho do arquivo desejado. Em seguida, usamos o método `String(contentsOfFile:encoding:)` para ler o conteúdo do arquivo e armazená-lo em uma string.

Agora, se o arquivo `nomes.txt` contiver a lista "Ana, João, Maria", a saída do código acima será:

```
Ana, João, Maria
```

## Aprofundando-se no assunto

Ao ler um arquivo de texto em Swift, é importante estar atento a alguns detalhes. Por exemplo, ao usar o método `contentsOfFile()`, devemos ter certeza de que o caminho do arquivo está correto e que o arquivo existe.

Além disso, é importante prestar atenção ao tipo de codificação do arquivo, que pode ser diferente de acordo com o idioma ou formato em que foi criado. No exemplo acima, usamos `.utf8`, mas existem outras opções disponíveis, como `.ascii` e `.unicode`.

Também é importante lembrar de sempre tratar possíveis erros ao ler um arquivo de texto, utilizando o `try-catch` como mostrado no exemplo.

## Veja também

Aqui estão alguns links úteis para aprender mais sobre como ler arquivos de texto em Swift:

- [Documentação oficial da Apple sobre o método `contentsOfFile()`](https://developer.apple.com/documentation/foundation/nsstring/1415784-contents)
- [Tutorial sobre leitura de arquivos em Swift](https://blog.nojaf.com/2015/06/26/reading-data-from-text-files-with-swift/)
- [Exemplo de uso do método `contentsOfFile()` em um app iOS](https://www.hackingwithswift.com/example-code/strings/reading-from-a-file-using-string-contentsoffile)