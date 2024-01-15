---
title:                "Trabalhando com csv"
html_title:           "Rust: Trabalhando com csv"
simple_title:         "Trabalhando com csv"
programming_language: "Rust"
category:             "Rust"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/rust/working-with-csv.md"
---

{{< edit_this_page >}}

## Por que

Se você precisa trabalhar com dados em formato CSV (Comma Separated Values), provavelmente está buscando uma forma eficiente e confiável de manipular essas informações. Com a linguagem de programação Rust, você pode facilmente gerenciar CSVs com segurança e desempenho.

## Como fazer

Para começar a trabalhar com CSVs em Rust, você precisará importar a biblioteca `csv` no seu código. Em seguida, é necessário definir o caminho do arquivo CSV que você deseja manipular.

```
use csv::Reader; // Importa a biblioteca csv
use std::fs::File; // Importa a biblioteca de manipulação de arquivos

fn main() {
  let file = File::open("dados.csv").unwrap(); // Define o caminho do arquivo CSV
  let mut reader = Reader::from_reader(file); // Inicializa o leitor do arquivo CSV
}
```

Agora que o arquivo foi aberto, você pode ler as linhas do CSV e realizar operações com os dados.

```
// Lê a primeira linha e imprime na tela
let result = reader.records().next().unwrap().unwrap();
println!("{:?}", result);

// Imprime o campo "Nome" da primeira linha
println!("{}", result.get(0).unwrap());
```

O código acima apresenta apenas uma pequena demonstração do que é possível fazer com a biblioteca `csv` em Rust. É importante lembrar que ao trabalhar com CSVs, é necessário lidar com possíveis erros, como valores nulos ou formatação incorreta.

## Mergulho profundo

Além das funções básicas de leitura e manipulação de dados, a biblioteca `csv` em Rust também oferece recursos avançados, como a possibilidade de definir delimitadores personalizados e trabalhar com tipos de dados personalizados. Além disso, a linguagem Rust tem um sistema de gerenciamento de memória que garante a segurança e eficiência no processamento de grandes volumes de dados.

## Veja também

- [Documentação da biblioteca csv em Rust](https://docs.rs/csv/latest/csv/) (em inglês)
- [Tutorial de como trabalhar com CSVs em Rust](https://www.youtube.com/watch?v=BmWrBwrWqwA) (em inglês)
- [Exemplos de código para manipular CSVs em Rust](https://github.com/BurntSushi/rust-csv/tree/master/examples) (em inglês)