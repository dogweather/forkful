---
title:                "Convertendo uma data em uma string"
html_title:           "Rust: Convertendo uma data em uma string"
simple_title:         "Convertendo uma data em uma string"
programming_language: "Rust"
category:             "Rust"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/rust/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## O que e por que?

Converter uma data em uma string é um processo comum na programação, onde uma data é transformada em um formato de texto legível para humanos. Os programadores geralmente realizam essa conversão para apresentar informações de data em uma interface amigável ao usuário ou para armazenar dados em um formato que possa ser facilmente comparado ou manipulado.

## Como fazer:

```Rust
use chrono::{DateTime, NaiveDate, Utc};

// Converter uma data atual em uma string
let now: DateTime<Utc> = Utc::now();
let string_date = now.format("%d-%m-%Y").to_string();

println!("{}", string_date); // imprime: 24-09-2021

// Converter uma data específica em uma string
let date = NaiveDate::from_ymd(1996, 8, 26);
let string_date2 = date.format("%d %B %Y").to_string();

println!("{}", string_date2); // imprime: 26 August 1996
```

## Mergulho profundo:

A conversão de uma data em uma string tem sido uma necessidade na programação desde os primeiros sistemas de computadores. Antes do uso de bibliotecas como a `chrono` em Rust, os programadores geralmente tinham que escrever seu próprio algoritmo para converter uma data em uma string. Outras alternativas para realizar essa tarefa em Rust incluem a biblioteca `time`, que usa formatação de strings semelhante à do `printf` em C.

## Veja também:

- [Documentação da biblioteca chrono](https://docs.rs/chrono/latest/chrono/)
- [Biblioteca time em Rust](https://crates.io/crates/time)
- [Tutorial de formatação de datas em Rust](https://stevedonovan.github.io/rust-dbv/dates.html)