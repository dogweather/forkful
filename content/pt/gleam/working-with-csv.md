---
title:                "Trabalhando com csv"
html_title:           "Gleam: Trabalhando com csv"
simple_title:         "Trabalhando com csv"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/gleam/working-with-csv.md"
---

{{< edit_this_page >}}

## Por que utilizar CSV no Gleam?

CSV (Comma-Separated Values) é um formato de arquivo amplamente utilizado para armazenar dados tabulares de forma simples e fácil de ser lida. Ao trabalhar com CSV no Gleam, você pode facilmente importar e exportar dados entre diferentes programas e bancos de dados. Além disso, o uso de CSV pode ajudar a manter a compatibilidade e interoperabilidade entre diferentes sistemas.

## Como usar CSV no Gleam

Você pode facilmente manipular arquivos CSV no Gleam usando as funções e módulos disponíveis na biblioteca padrão. Abaixo está um exemplo simples de como ler e imprimir os dados de um arquivo CSV:

```Gleam
import csv

let reader = csv.new_reader("arquivo.csv")
for row in reader do
  io.println(row)
pub fn main() {
  ok
}
```

O arquivo CSV utilizado no exemplo possui três colunas: nome, idade e cidade. O código acima irá imprimir cada linha do arquivo, exibindo o nome da pessoa, sua idade e sua cidade.

## Mergulho Profundo em CSV

Ao trabalhar com arquivos CSV no Gleam, é importante ter em mente que esses arquivos podem conter valores nulos ou vazios, o que pode causar erros ao manipulá-los. Portanto, é importante estar ciente disso ao escrever o seu código.

Além disso, a biblioteca padrão do Gleam também oferece a possibilidade de escrever dados em arquivos CSV, utilizando a função `csv.new_writer()`.

## Veja também

- Documentação oficial do CSV no Gleam: [https://gleam.run/documentation/standard-library/csv](https://gleam.run/documentation/standard-library/csv)
- Tutorial sobre manipulação de arquivos CSV no Gleam: [https://medium.com/gleam-lang/manipulating-csv-files-in-gleam-26a95378386d](https://medium.com/gleam-lang/manipulating-csv-files-in-gleam-26a95378386d)