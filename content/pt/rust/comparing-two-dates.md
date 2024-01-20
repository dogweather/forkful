---
title:                "Comparando duas datas"
html_title:           "C#: Comparando duas datas"
simple_title:         "Comparando duas datas"
programming_language: "Rust"
category:             "Rust"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/rust/comparing-two-dates.md"
---

{{< edit_this_page >}}

# Rust: Como Comparar Duas Datas

## O Que & Porquê?

Comparar duas datas significa verificar qual data é mais recente ou se são iguais. Fazemos isso em situações nas quais precisamos ordenar eventos no tempo, marcar prazos, calcular idades, etc.

## Como fazer:

Vamos usar a biblioteca Chrono para manipulação de datas em Rust. Primeiro, você precisa adicioná-la às suas dependências:

```Rust
[dependencies]
chrono = "0.4"
```

Em seguida, importe a biblioteca e compare duas datas:

```Rust
extern crate chrono;
use chrono::{NaiveDate, Datelike};

fn main() {
    let data1 = NaiveDate::from_ymd(2021, 11, 23);
    let data2 = NaiveDate::from_ymd(2020, 11, 23);

    if data1 > data2 {
        println!("{} é depois de {}", data1, data2);
    } else if data1 < data2 {
        println!("{} é antes de {}", data1, data2);
    } else {
        println!("As datas são iguais");
    }
}
```
Resultado esperado:

```Rust
"2021-11-23 é depois de 2020-11-23"
```

## Mergulho Profundo

A Chrono é uma biblioteca de Rust que tem suas raízes no pacote time do Rust 0.1. Este pacote era um wrapper em torno de gmtime e localtime do C, mas a biblioteca se expandiu para incluir muitas mais funcionalidades.

Uma alternativa seria escrever nossa lógica de comparação de datas, mas essa seria uma abordagem propensa a erros e potencialmente redundante. A Chrono tem uma equipe dedicada que se esforça para manter a precisão em todas as operações de tempo, então é muito menos provável que surgirem problemas ao usá-la.

A lógica de comparação da Chrono baseia-se na implementação do trait `PartialOrd` para `NaiveDate`. Este trait especifica a operação que permite a comparação de itens. No caso de `NaiveDate`, isso é feito comparando ano, mês e dia em ordem.

## Ver Também

- [Documentação Chrono](https://docs.rs/chrono/0.4.11/chrono/) 