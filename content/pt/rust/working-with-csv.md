---
title:                "Trabalhando com CSV"
html_title:           "Bash: Trabalhando com CSV"
simple_title:         "Trabalhando com CSV"
programming_language: "Rust"
category:             "Rust"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/rust/working-with-csv.md"
---

{{< edit_this_page >}}

## O Que é & Porquê?

Trabalhar com CSV (Comma-Separated Values, ou Valores Separados por Vírgula) significa manipular dados em um formato texto simples, muito usado por ser de fácil leitura e escrita para humanos e máquinas. Programadores usam CSV para importar e exportar dados de sistemas, fazer análise de dados, transferir informações entre diferentes programas, entre outros.

## Como Fazer:

```Rust
use std::error::Error;
use csv;
use serde::{Deserialize, Serialize};

// Definir a estrutura para representar uma linha do CSV
#[derive(Debug, Serialize, Deserialize)]
struct Record {
    nome: String,
    idade: u32,
    cidade: String,
}

fn ler_csv() -> Result<(), Box<dyn Error>> {
    let mut rdr = csv::Reader::from_path("pessoas.csv")?;
    for result in rdr.deserialize() {
        let record: Record = result?;
        println!("{:?}", record);
    }
    Ok(())
}

fn escrever_csv() -> Result<(), Box<dyn Error>> {
    let mut wtr = csv::Writer::from_path("saida.csv")?;
    wtr.serialize(Record { nome: "Ana".into(), idade: 25, cidade: "Lisboa".into() })?;
    Ok(())
}

fn main() {
    if let Err(err) = ler_csv() {
        println!("Erro ao ler CSV: {}", err);
    }
    
    if let Err(err) = escrever_csv() {
        println!("Erro ao escrever CSV: {}", err);
    }
}
```

## Mergulho Profundo

O formato CSV foi popularizado na década de 70 e continua relevante pela simplicidade e interoperabilidade. Embora o JSON e o XML ofereçam alternativas mais ricas em termos de estrutura e dados, o CSV mantém sua posição pela facilidade de uso com ferramentas de planilha como Excel e Google Sheets. Rust oferece ótimas bibliotecas como `csv` e `serde` para lidar com CSVs, permitindo deserialização e serialização de structs de forma quase automática.

## Veja Também

- Documentação da crate CSV para Rust: [https://docs.rs/csv](https://docs.rs/csv)
- Guia da linguagem de programação Rust: [https://www.rust-lang.org/pt-BR/learn](https://www.rust-lang.org/pt-BR/learn)
- Tutorial Serde Rust: [https://serde.rs](https://serde.rs)
