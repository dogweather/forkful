---
title:                "Trabalhando com csv"
html_title:           "Clojure: Trabalhando com csv"
simple_title:         "Trabalhando com csv"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/clojure/working-with-csv.md"
---

{{< edit_this_page >}}

## Por que trabalhar com CSV?

CSV (Comma Separated Values) é um formato de arquivo extremamente popular para armazenar dados tabulares, como planilhas ou tabelas de banco de dados. Ao trabalhar com CSV, é possível facilmente importar e exportar dados em diferentes ferramentas, tornando-o uma escolha conveniente para projetos de análise de dados ou integração de sistemas.

## Como usar CSV em Clojure

Para começar a trabalhar com CSV em Clojure, é necessário primeiro importar a biblioteca `clojure-csv`.

```Clojure
(require '[clojure-csv.core :as csv])
```

Em seguida, é preciso ler o arquivo CSV usando a função `parse-csv` e especificando o caminho do arquivo como um argumento:

```Clojure
(let [file (-> "caminho/do/arquivo.csv" (clojure.java.io/resource) (.getFile))]
  (with-open [reader (clojure.java.io/reader file)]
    (csv/parse-csv reader)))
```

Isso retornará uma lista de listas, ou seja, cada linha do arquivo CSV se tornará uma lista dentro da lista principal. Se o arquivo possuir um cabeçalho, é possível utilizar a opção `:keys true` para ler os dados como um mapa:

```Clojure
(csv/parse-csv reader :keys true)
```

Para escrever em um arquivo CSV, basta usar a função `write-csv` e especificar o caminho do arquivo e os dados a serem escritos:

```Clojure
(csv/write-csv "caminho/do/novoarquivo.csv" 
               [["Nome" "Idade"]
               ["João" 25]
               ["Maria" 30]])
```

A saída será um arquivo CSV com duas colunas, "Nome" e "Idade", e duas linhas de dados, "João" e 25, e "Maria" e 30.

## Mergulho profundo em CSV

Ao trabalhar com CSV, é importante entender que as células de dados podem ser separadas por diferentes delimitadores, não apenas vírgulas. É possível especificar o delimitador a ser usado ao ler ou escrever em um arquivo CSV, através do argumento `:separator`.

Além disso, é importante também lidar com possíveis erros, como células de dados com aspas. É possível usar a opção `:quote` para definir um caractere de citação para lidar com esse tipo de situação.

```Clojure
(csv/parse-csv reader :keys true :separator \; :quote \")
```

No exemplo acima, foi especificado o separador como ponto e vírgula e as aspas como caractere de citação.

## Veja também

- [Documentação oficial de Clojure CSV](https://github.com/davidsantiago/clojure-csv)
- [Tutorial de Clojure em Português](https://github.com/Clojure-BR/clojure-br.github.com)