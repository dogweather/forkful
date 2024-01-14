---
title:                "Rust: Calculando uma data no futuro ou passado"
simple_title:         "Calculando uma data no futuro ou passado"
programming_language: "Rust"
category:             "Rust"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/rust/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Por que calcular uma data no futuro ou passado?

A programação em Rust é conhecida por sua combinação de velocidade e segurança. Ao calcular uma data no futuro ou passado, os programadores podem aproveitar essas vantagens, garantindo que seus códigos sejam eficientes e sem erros.

## Como calcular uma data no futuro ou passado em Rust

Para calcular uma data no futuro ou passado em Rust, precisamos usar o tipo de dados "Date" da biblioteca "chrono". Para isso, primeiro precisamos adicionar a dependência "chrono" ao nosso arquivo "Cargo.toml".

```
[dependencies]
chrono = { package = "chrono", version = "0.4" }
```
Em seguida, importamos a biblioteca em nosso código:

```
use chrono::{NaiveDate, Duration};
```
Agora podemos definir uma data específica e adicionar um número de dias a ela, usando o método "checked_add_signed":

```
let data = NaiveDate::from_ymd(2021, 10, 15);
let data_no_futuro = data.checked_add_signed(Duration::days(7));
println!("Data no futuro: {:?}", data_no_futuro);
```

A saída do código acima será: "Data no futuro: Some(2021-10-22)".

## Deep Dive: Calculando uma data no futuro ou passado em detalhes

Ao calcular uma data no futuro ou passado em Rust, existem algumas coisas importantes a considerar. Primeiro, o tipo de dados "Date" não leva em conta o fuso horário, portanto, é importante levar isso em consideração ao definir as datas. Além disso, precisamos ter cuidado para não somar ou subtrair um número muito grande de dias, pois pode causar um estouro de memória.

A biblioteca "chrono" oferece outros métodos úteis para manipular datas, como "signed_duration_since", que calcula a diferença entre duas datas em dias ou "sub_signed", que subtrai uma duração de uma data. Explorar e entender esses métodos pode ajudar a tornar seus códigos de cálculo de datas mais eficientes e precisos.

## Veja também

- Documentação da biblioteca "chrono" em Rust: https://docs.rs/chrono/0.4.19/chrono/
- Tutorial sobre como calcular datas em Rust: https://blog.logrocket.com/handling-dates-in-rust-with-chrono/