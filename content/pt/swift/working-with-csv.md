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

## O Que e Por Que?

Trabalhar com CSV pode ser um elemento muito importante na programação. O CSV (Comma Separated Values) é um formato de arquivo comum para armazenar e compartilhar dados de forma estruturada, onde cada valor é separado por uma vírgula. Os programadores usam o CSV porque é um formato simples e fácil de ler e editar, tornando-o útil para armazenar grandes quantidades de dados.

## Como Fazer:

Para começar a trabalhar com CSV no Swift, primeiro você precisa importar o framework SwiftCSV para o seu projeto. Em seguida, você pode carregar um arquivo CSV existente usando a função `loadCSV()` e acessar os dados usando a propriedade `rows` da classe `CSV`.

```
import SwiftCSV

// Carregar o arquivo CSV
let csv = loadCSV(path: "caminho/do/arquivo.csv")

// Acessar os dados
let dados = csv.rows
```

Você também pode criar um CSV a partir de um array de arrays de strings e depois salvá-lo em um arquivo usando a função `saveCSV()`.

```
import SwiftCSV

// Criar CSV a partir de array de arrays de strings
let dados = [["Nome", "Idade"], ["Maria", "25"], ["João", "30"], ["Ana", "20"]]
let csv = CSV(dados: dados)

// Salvá-lo em um arquivo
saveCSV(csv: csv, path: "caminho/do/novoArquivo.csv")
```

## Mergulho Profundo:

O formato CSV foi criado em 1972 para facilitar o compartilhamento de dados entre diferentes programas e sistemas. Ele é amplamente utilizado atualmente, especialmente para importação e exportação de dados em aplicativos de planilhas e bancos de dados. Alguns dos formatos alternativos para armazenar dados são JSON, XML e YAML, mas o CSV continua sendo uma opção popular devido à sua simplicidade.

Ao trabalhar com CSV no Swift, é importante ter em mente que os dados não são tipados, o que significa que você precisa converter os valores para o tipo apropriado antes de usá-los em seu código.

## Veja Também:

- [Artigo sobre o histórico do formato CSV](https://en.wikipedia.org/wiki/Comma-separated_values)