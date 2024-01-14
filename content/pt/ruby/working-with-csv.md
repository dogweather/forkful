---
title:                "Ruby: Manuseando arquivos csv"
simple_title:         "Manuseando arquivos csv"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/ruby/working-with-csv.md"
---

{{< edit_this_page >}}

## Por que

Trabalhar com arquivos CSV (Valores Separados por Vírgula) é uma tarefa comum para muitos programadores. CSV é um formato simples e fácil de usar para armazenar dados tabulares. Ao dominar a manipulação de arquivos CSV, você pode facilmente importar e exportar dados para diversas aplicações e ferramentas.

## Como Fazer

Para trabalhar com arquivos CSV em Ruby, há algumas bibliotecas que podem ser utilizadas, como "csv", "smarter_csv", "fastercsv" e "pandas". Abaixo, segue um exemplo de leitura e exibição de dados de um arquivo CSV:

```Ruby
require 'csv'

CSV.foreach("dados.csv") do |linha|
    puts linha.inspect
end
```

O código acima irá percorrer o arquivo "dados.csv" e imprimir todos os dados contidos nele. Você também pode especificar colunas específicas para serem exibidas, utilizando o operador "[]" e o índice correspondente à coluna desejada.

## Mergulho Profundo

Ao trabalhar com arquivos CSV, é importante ter em mente que nem sempre os dados estarão no formato esperado. É comum encontrarmos dados ausentes, informações duplicadas ou formatação incorreta. Por isso, é importante validar os dados antes de utilizá-los em suas aplicações.

Outro ponto importante é definir a codificação correta do arquivo CSV. Muitos arquivos são salvos em codificação UTF-8, mas pode haver casos em que é necessário definir uma codificação específica para garantir a correta exibição dos dados.

## Veja Também

- Ruby CSV library documentation: https://ruby-doc.org/stdlib-2.7.0/libdoc/csv/rdoc/CSV.html
- Smarter CSV gem: https://github.com/tilo/smarter_csv
- Pandas library for advanced data manipulation: https://pandas.pydata.org/