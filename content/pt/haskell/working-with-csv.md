---
title:                "Haskell: Trabalhando com csv"
simple_title:         "Trabalhando com csv"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/haskell/working-with-csv.md"
---

{{< edit_this_page >}}

## Por que trabalhar com CSV em Haskell?

CSV (Comma-Separated Values) é um formato de arquivo amplamente utilizado para armazenar dados tabulares de forma simples e fácil de entender. Em Haskell, trabalhar com CSV pode ser uma habilidade valiosa, especialmente quando se lida com conjuntos de dados grandes e complexos. Além disso, é uma ótima maneira de praticar e melhorar suas habilidades de programação funcional.

## Como fazer

A seguir, mostraremos como você pode trabalhar com CSV em Haskell usando algumas bibliotecas populares, como "Data.ByteString.Lazy" e "Data.CSV". Primeiro, você precisará importá-las em seu código:

```Haskell
import qualified Data.ByteString.Lazy as BL
import qualified Data.CSV as CSV
```

### Lendo um arquivo CSV

Para ler um arquivo CSV, você pode usar a função "parseCSVFromFile" da biblioteca Data.CSV. Ela lê um arquivo CSV e retorna uma lista de linhas, que por sua vez são listas de campos separados por vírgulas.

```Haskell
main = do
  rawData <- BL.readFile "dados.csv" -- Lê o arquivo
  let csvData = CSV.parseCSVFromFile "dados.csv" rawData -- Faz o parsing do arquivo
  print csvData
```

### Manipulando dados CSV

Uma vez que você tenha os dados do arquivo CSV formatados, você pode manipulá-los da maneira desejada. Por exemplo, digamos que você queria imprimir todos os campos da primeira linha do seu arquivo CSV:

```Haskell
printLine :: CSV.Record -> IO ()
printLine record = do
  putStrLn $ "Primeira linha do arquivo: " ++ (show $ record !! 0) -- Imprime o primeiro campo
  putStrLn $ "Segunda linha do arquivo: " ++ (show $ record !! 1) -- Imprime o segundo campo
  ...

main = do
  rawData <- BL.readFile "dados.csv"
  let Right csvData = CSV.parseCSVFromFile "dados.csv" rawData -- Como a parseCSVFromFile retorna um Either, usamos o "Right" para pegar o valor caso não haja erros
  mapM_ printLine (CSV.records csvData) -- Chama a função printLine para cada linha do arquivo
```

## Detalhes sobre trabalhando com CSV em Haskell

Ao trabalhar com CSV em Haskell, é importante lembrar que os dados serão lidos como "ByteStrings" ou "Strings". Isso significa que você precisará convertê-los para os tipos de dados desejados antes de manipulá-los. Você também pode querer considerar o uso de funções de manipulação de listas, como "map" e "filter", para trabalhar com os dados de forma mais eficiente.

Outro aspecto importante a ser considerado é a detecção e tratamento de possíveis erros no arquivo CSV, como linhas vazias ou campos ausentes. Para isso, você pode usar funções como "isRecordValid" e "combineErrorValidation" da biblioteca Data.CSV.

## Veja também

- [Documentação oficial da biblioteca Data.CSV] (https://hackage.haskell.org/package/csv)
- [Tutorial sobre trabalhando com CSV em Haskell] (https://www.parsonsmatt.org/2018/05/19/csv_haskell.html)
- [Outros exemplos de código para manipulação de CSV em Haskell] (https://github.com/search?l=Haskell&q=csv&type=Repositories)