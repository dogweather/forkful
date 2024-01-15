---
title:                "Comparando duas datas"
html_title:           "Rust: Comparando duas datas"
simple_title:         "Comparando duas datas"
programming_language: "Rust"
category:             "Rust"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/rust/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Por que

Comparar duas datas pode ser uma tarefa muito comum em programação, especialmente quando lidamos com informações que envolvem tempo e datas cronológicas. Com o Rust, podemos realizar essa tarefa de forma eficiente e segura, garantindo que nossos programas funcionem corretamente.

## Como Fazer

```rust
fn main() {
    let data1 = "2021-08-30"; //primeira data
    let data2 = "2021-09-05"; //segunda data

    if data1 < data2 {
        println!("A primeira data é anterior à segunda data");
    } else if data1 > data2 {
        println!("A primeira data é posterior à segunda data");
    } else {
        println!("As duas datas são iguais");
    }
}
```

**Saída:**

A primeira data é anterior à segunda data

Neste exemplo, usamos o operador de comparação "menor que" ( < ) e "maior que" ( > ) para comparar as duas datas. Se a primeira data for anterior à segunda, a condição `if` será verdadeira e a primeira mensagem será impressa. Se a primeira data for posterior à segunda, a condição do `else if` será verdadeira e a segunda mensagem será impressa. Caso as datas sejam iguais, a condição do `else` será verdadeira e a terceira mensagem será impressa.

## Aprofundando

A comparação de datas em Rust é baseada em strings, o que significa que o Rust verifica a ordem alfabética das strings para determinar a comparação de datas. Portanto, ao comparar datas, devemos garantir que elas estejam no formato "aaaa-mm-dd" para que a ordem seja corretamente avaliada.

Além disso, o Rust também possui a estrutura `chrono` que nos permite trabalhar com datas de forma mais precisa e realizar operações como cálculos de diferenças entre datas e conversão entre diferentes formatos. Você pode ler mais sobre `chrono` e sua documentação [aqui](https://docs.rs/chrono/).

## Veja também

- [Diferenças entre datas com Rust](https://crates.io/crates/chrono)
- [Operadores de comparação em Rust](https://doc.rust-lang.org/book/ch03-02-data-types.html#comparison-operators)