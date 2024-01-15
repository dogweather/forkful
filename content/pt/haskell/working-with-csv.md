---
title:                "Trabalhando com csv"
html_title:           "Haskell: Trabalhando com csv"
simple_title:         "Trabalhando com csv"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/haskell/working-with-csv.md"
---

{{< edit_this_page >}}

## Por que trabalhar com CSV em Haskell?

CSV (Comma-Separated Values) é um formato de arquivo amplamente utilizado para armazenar e compartilhar dados. Ao trabalhar com dados em projetos de programação, muitas vezes nos deparamos com a necessidade de lidar com CSV. Em Haskell, existem diversas bibliotecas e funções que facilitam o manuseio desses arquivos, tornando o processo mais eficiente e organizado.

## Como fazer

Para trabalhar com CSV em Haskell, precisamos importar a biblioteca `csv` e a função `parseCSV` que faz parte dessa biblioteca. Em seguida, podemos utilizar a função `parseCSVFromFile` para ler um arquivo CSV existente e transformá-lo em uma matriz de valores do tipo `[[Field]]`, onde `Field` é uma string.

```Haskell
import Text.CSV 

-- Le o arquivo CSV e retorna uma matriz de valores
csvData <- parseCSVFromFile "arquivo.csv" 

-- Acessando o valor na segunda linha e terceira coluna
let valor = csvData !! 1 !! 2
```

Podemos também utilizar a função `printCSV` para imprimir os dados em formato CSV. Se quisermos escrever dados em um novo arquivo CSV, podemos utilizar a função `writeCSV`. Para facilitar a leitura e escrita de dados em formato CSV, podemos usar operações de listas e outras funções auxiliares.

## Mergulho profundo

Além das funções mencionadas acima, a biblioteca `csv` também oferece outras ferramentas úteis para o trabalho com CSV em Haskell. Podemos utilizar a função `parseCSVWithHeaders` para ler um arquivo CSV com cabeçalhos e obter uma lista de dicionários com os dados. Outra função interessante é `mapRows`, que permite aplicar uma função a cada linha do arquivo CSV.

Além disso, Haskell possui outras bibliotecas que podem ser úteis para trabalhar com CSV, como `cassava`, `haskell-csv` e `haskell-data-csv`.

## Veja também

- [Documentação da biblioteca `csv`](https://hackage.haskell.org/package/csv)
- [Tutorial de como trabalhar com CSV em Haskell](https://riptutorial.com/haskell/example/14364/working-with-csv-files)
- [Tutorial de como ler e escrever arquivos CSV com a biblioteca `cassava`](https://www.snoyman.com/blog/2016/10/bare-bones-csv)