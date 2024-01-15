---
title:                "Trabalhando com csv"
html_title:           "Swift: Trabalhando com csv"
simple_title:         "Trabalhando com csv"
programming_language: "Swift"
category:             "Swift"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/swift/working-with-csv.md"
---

{{< edit_this_page >}}

## Por que trabalhar com CSV?

O CSV (Comma-Separated Values) é um formato muito popular para armazenar dados em formato de tabela, com valores separados por vírgulas. Trabalhar com CSV é importante para ler e gravar dados em diferentes formatos de arquivo, como planilhas e bancos de dados, e é um recurso essencial para desenvolvedores que trabalham com dados.

## Como fazer:

Para trabalhar com CSV em Swift, é necessário importar a biblioteca "Foundation", que já vem incluída na linguagem. Em seguida, basta utilizar a função "String(contentsOfFile:encoding:)" para ler o arquivo CSV e a função "write(toFile:atomically:encoding:)" para escrever no arquivo. Veja um exemplo:

```Swift
import Foundation

// Lendo arquivo CSV
if let fileURL = Bundle.main.url(forResource: "dados", withExtension: "csv") {
  do {
    let content = try String(contentsOfFile: fileURL.path, encoding: .utf8)
    print(content) // imprime o conteúdo do arquivo CSV

  } catch {
    print(error)
  }
}

// Escrevendo arquivo CSV
let stringCSV = "Nome, Sobrenome, Idade\nJoão, Silva, 30\nMaria, Souza, 25"
let fileManager = FileManager.default
let file = "dados.csv"
let directory = fileManager.urls(for: .documentDirectory, in: .userDomainMask).first!
let fileURL = directory.appendingPathComponent(file)
do {
    try stringCSV.write(to: fileURL, atomically: true, encoding: .utf8)
} catch {
    print(error)
}
```

O resultado do código acima seria a leitura do conteúdo do arquivo CSV e a criação de um novo arquivo CSV com os dados fornecidos.

## Mais sobre CSV:

Além das funções mencionadas, o Swift oferece outras opções para trabalhar com CSV, como a biblioteca externa "CSVImporter", que facilita a leitura e a escrita de arquivos CSV com diferentes delimitadores e opções de formatação. Também é possível acessar cada valor em um arquivo CSV e manipulá-lo individualmente, além de realizar operações como ordenar, filtrar e agrupar os dados contidos no CSV.

## Veja também:

- [Documentação oficial do Swift sobre leitura e gravação de arquivos](https://developer.apple.com/documentation/foundation/filemanager)
- [CSVImporter para manipulação avançada de arquivos CSV em Swift](https://github.com/vincode-io/CSVImporter)
- [Tutorial sobre como trabalhar com CSV em Swift](https://medium.com/@abhimuralidharan/working-with-csv-files-in-swift-5-ios-8a6572aa80f)