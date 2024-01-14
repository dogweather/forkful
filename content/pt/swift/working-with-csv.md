---
title:                "Swift: Trabalhando com arquivos csv"
simple_title:         "Trabalhando com arquivos csv"
programming_language: "Swift"
category:             "Swift"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/swift/working-with-csv.md"
---

{{< edit_this_page >}}

## Por que trabalhar com CSV em Swift?

CSV (Comma-Separated Values) é um formato de arquivo muito comum usado para armazenar e trocar dados entre diferentes sistemas. Ao trabalhar com CSV em Swift, você poderá facilmente importar e exportar dados em uma variedade de aplicativos e plataformas.

## Como fazer isso em Swift?

Para trabalhar com CSV em Swift, você precisará da estrutura `String` padrão do Swift. Depois de importar seus dados CSV como uma string, você poderá usar o método `components(separatedBy:)` para dividir a string em um array de strings. Aqui está um exemplo de código que ilustra isso:

```
let csvData = """
1,João,35
2,Maria,28
3,José,42
"""

let array = csvData.components(separatedBy: "\n")
print(array)
```

A saída desse código será um array com três elementos, cada um contendo uma linha do arquivo CSV. Agora você pode usar métodos como `substring(from:)` ou `components(separatedBy:)` novamente para obter os valores individuais de cada linha.

## Aprofundando

Além do método básico de divisão de strings, existem muitas bibliotecas e frameworks disponíveis para trabalhar com CSV em Swift. Um exemplo comum é o `CSV.swift`, que possui uma sintaxe fácil de usar para leitura e escrita de dados CSV.

Outra consideração importante ao trabalhar com CSV é o tratamento de erros. Ao importar dados CSV, é importante ter certeza de que os valores estão no formato correto e de que nenhuma informação importante é perdida durante o processo de divisão de strings. Isso pode ser feito usando declarações `guard let` ou usando uma biblioteca como o `SwiftCSV`, que possui recursos de validação de dados embutidos.

## Veja também

- [Documentação oficial do Swift](https://swift.org/)
- [Biblioteca CSV.swift](https://github.com/yaslab/CSV.swift)
- [Biblioteca SwiftCSV](https://github.com/naoty/SwiftCSV)

Agora que você sabe como trabalhar com CSV em Swift, experimente e descubra suas próprias maneiras de aproveitar ao máximo esse formato de arquivo versátil.