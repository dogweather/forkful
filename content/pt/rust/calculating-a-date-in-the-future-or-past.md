---
title:                "Calculando uma data no futuro ou no passado"
html_title:           "Rust: Calculando uma data no futuro ou no passado"
simple_title:         "Calculando uma data no futuro ou no passado"
programming_language: "Rust"
category:             "Rust"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/rust/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

# Cálculo de Datas: Futuro e Passado com Rust
Neste artigo, vamos tratar do "cálculo de datas" em Rust. Vamos compreender como calcular uma data futura ou passada, além de aprofundar os conceitos envolvidos.

## O Que e Por Quê?
O cálculo de datas é útil para resolver problemas comuns no mundo da programação, como calcular prazos, verificar a validade de um certificado, ou qualquer situação que envolva a manipulação de datas e horas.

## Como Fazer
Abordaremos dois cenários comuns: calcular uma data futura e calcular uma data passada. Para esses casos, vamos usar a biblioteca `chrono` do Rust.

```Rust
// Adicionando a biblioteca chrono ao seu projeto
[dependencies]
chrono = "0.4"
```
Para calcular uma data futura:

```Rust
extern crate chrono;
use chrono::{DateTime, Duration, Utc};

fn main() {
    let agora = Utc::now();
    let futuro = agora + Duration::days(10);
    
    println!("{}", futuro);
}
```
E para calcular uma data passada:

```Rust
extern crate chrono;
use chrono::{DateTime, Duration, Utc};

fn main() {
    let agora = Utc::now();
    let passado = agora - Duration::days(10);
    
    println!("{}", passado);
}
```

## Aprofundamento 
Em termos de contexto histórico, a abordagem de cálculo de datas que usamos aqui está em linha com muitas outras linguagens de programação, como Python e Java, que fornecem bibliotecas semelhantes para manipulação de datas e horas.

Falando sobre alternativas, sempre é possível trabalhar diretamente com timestamps. No entanto, a abordagem com a `chrono` é geralmente preferida por ser mais legível e menos propensa a erros.

Quanto aos detalhes de implementação, a `chrono` utiliza uma estrutura de dados para consolidação de informações de data e hora. O método `now` retorna a data e hora atual, enquanto os métodos `Duration::days`, somado ou subtraído de uma data, permitem calcular uma data no futuro ou no passado.

## Veja Também
Para mais recursos sobre o tópico, consulte:

* A documentação oficial da biblioteca `chrono`: [Chrono](https://docs.rs/chrono/0.4.19/chrono/)
* A seção de datas e horas do 'The Rust Programming Language': [Rust Book](https://doc.rust-lang.org/book/ch02-00-guessing-game-tutorial.html)