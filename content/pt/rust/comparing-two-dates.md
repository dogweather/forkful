---
title:                "Rust: Comparando duas datas"
programming_language: "Rust"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/rust/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Porquê

Comparar datas pode ser uma tarefa comum quando se trabalha com programação. No entanto, pode ser desafiador encontrar uma maneira eficiente e precisa de realizar essa comparação. Neste artigo, vamos explorar como a linguagem de programação Rust pode nos ajudar a comparar duas datas de forma eficiente e confiável.

## Como Fazer

Em Rust, podemos utilizar o tipo `DateTime` do crate `chrono` para representar uma data e hora específica. Para comparar duas datas, podemos utilizar o método `.cmp()`, que retorna um enum `Ordering`. Vamos ver um exemplo prático:

```Rust
use chrono::{DateTime, Utc};

fn main() {
    let data1 = DateTime::parse_from_rfc3339("2021-01-01T00:00:00Z").unwrap();
    let data2 = DateTime::parse_from_rfc3339("2021-01-02T00:00:00Z").unwrap();
    
    match data1.cmp(&data2) {
        Ordering::Less => println!("A data 1 é anterior à data 2."),
        Ordering::Equal => println!("As datas são iguais."),
        Ordering::Greater => println!("A data 1 é posterior à data 2.")
    }
}
```

A saída deste programa será:

```
A data 1 é anterior à data 2.
```

Podemos também utilizar os métodos `.lt()`, `.eq()` e `.gt()` para realizar comparações específicas. Além disso, o crate `chrono` possui uma série de funções úteis para realizar cálculos e manipulações com datas. Certifique-se de consultar a documentação oficial para obter mais informações.

## Mergulho Profundo

Ao comparar duas datas em Rust, é importante ter em mente que estamos comparando não apenas o dia e o mês, mas também o ano e a hora. Isso significa que devemos ser cuidadosos ao trabalhar com fusos horários e dados que podem estar em diferentes formatos de data. Além disso, é importante entender como Rust lida com a comparação de tipos diferentes, como `DateTime` e `Date`.

## Veja Também

- Documentação do crate `chrono`: https://docs.rs/chrono/
- Tutorial "Trabalhando com Datas em Rust": https://stevedonovan.github.io/rust-gentle-intro/7-dates-times.html