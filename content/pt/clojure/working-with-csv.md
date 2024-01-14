---
title:                "Clojure: Trabalhando com csv"
simple_title:         "Trabalhando com csv"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/clojure/working-with-csv.md"
---

{{< edit_this_page >}}

## Por que trabalhar com CSV em Clojure?

O formato CSV (Comma-Separated Values) é amplamente utilizado para armazenar dados tabulares, como planilhas e bancos de dados. Trabalhar com CSV em Clojure pode ser uma ótima opção para processamento de dados ou integração com outros sistemas. As ferramentas e bibliotecas disponíveis em Clojure tornam a manipulação de dados em formato CSV rápida e eficiente.

## Como fazer

Para trabalhar com CSV em Clojure, podemos utilizar a biblioteca clojure.data.csv, que já vem pré-instalada com a linguagem. Com ela, podemos ler e escrever arquivos CSV, além de manipular seus dados de forma simples e eficiente.

**Lendo um arquivo CSV:**

```
(require '[clojure.data.csv :as csv])

(with-open [file (clojure.java.io/reader "exemplo.csv")]
  (csv/read-csv file))
```

**Exemplo de saída:**

```
(("Nome" "Idade" "Profissão")
("Maria" "30" "Engenheira")
("João" "25" "Programador")
("Ana" "28" "Advogada"))
```

**Escrevendo em um arquivo CSV:**

```
(require '[clojure.data.csv :as csv])

(def dados [["Nome" "Idade" "Profissão"]
            ["Maria" "30" "Engenheira"]
            ["João" "25" "Programador"]
            ["Ana" "28" "Advogada"]])

(with-open [file (clojure.java.io/writer "exemplo.csv")]
  (csv/write-csv file dados))
```

**Exemplo de saída:**

```
Nome,Idade,Profissão
Maria,30,Engenheira
João,25,Programador
Ana,28,Advogada
```

Além desses exemplos básicos, a biblioteca clojure.data.csv oferece ainda outras funções para manipulação de dados em formato CSV, como ordenação, filtragem, adição e remoção de colunas, entre outras.

## Informações avançadas

Trabalhar com CSV em Clojure pode ser ainda mais aprofundado com o uso de bibliotecas externas, como clojure-csv, que oferece outras funcionalidades para manipulação de dados tabulares.

Uma outra opção interessante é o uso de ferramentas como clojure.data.csv.core e clojure.data.csv/parse, que permitem a leitura e escrita de dados em formato CSV diretamente a partir da memória, sem a necessidade de trabalhar com arquivos externos. Isso pode ser útil para processamento de grandes quantidades de dados ou para integração com outros sistemas.

## Veja também

- Documentação oficial da biblioteca clojure.data.csv: https://clojure.github.io/data.csv/
- Biblioteca clojure-csv: https://github.com/davl/clj-csv
- Outras ferramentas de manipulação de CSV em Clojure: https://planet.clojure.in/displaying-a-csv-file-in-html-using-clojure-and-compojure