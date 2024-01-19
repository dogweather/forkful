---
title:                "Analisando uma data a partir de uma string"
html_title:           "PowerShell: Analisando uma data a partir de uma string"
simple_title:         "Analisando uma data a partir de uma string"
programming_language: "Rust"
category:             "Rust"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/rust/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## O quê & Por quê?
Analisar uma data de uma string significa extrair informações de data e hora de um texto. Programadores o fazem para transformar a data legível por humanos em um formato mais útil para processamento de computador.

## Como fazer:
Aqui está um exemplo de como analisar uma data de uma string em Rust:
```Rust
use chrono::{NaiveDate, FormatError};

fn parse_date_from_str(date_str: &str) -> Result<NaiveDate, FormatError> {
    NaiveDate::parse_from_str(date_str, "%Y-%m-%d")
}

fn main() {
    let date_str = "2022-03-01";
    let date = parse_date_from_str(date_str);
    match date {
        Ok(d) => println!("Data: {}", d),
        Err(e) => println!("Erro ao analisar data: {}", e),
    }
}
```
Quando executado, o código acima imprimirá "Data: 2022-03-01".

## Mergulho profundo:
Historicamente, em Rust, analisamos datas de strings usando a biblioteca `chrono`. Embora outras alternativas estejam disponíveis (como `time`), `chrono` oferece uma ampla variedade de recursos e é comumente usada na comunidade Rust.

Ao analisar uma data, `chrono` verifica a correspondência entre a string e o formato fornecido. Se a string puder ser mapeada para esse formato, um objeto `NaiveDate` é retornado. Caso contrário, um erro é gerado. Este processo é fundamental para garantir a validade dos dados.

## Veja também:
1. Documentação oficial do Chrono: https://docs.rs/chrono/0.4.19/chrono/
2. Guia da API Rust: https://rust-lang.github.io/rust-cookbook/dates/times.html
3. Conteúdo relacionado no Stack Overflow: https://stackoverflow.com/questions/27314397/how-to-parse-a-string-into-a-datetime-in-rust