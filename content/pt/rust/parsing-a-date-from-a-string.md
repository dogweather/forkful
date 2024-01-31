---
title:                "Analisando uma data a partir de uma string"
date:                  2024-01-20T15:38:44.743233-07:00
simple_title:         "Analisando uma data a partir de uma string"

tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/rust/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## O Que & Por Que?
Analisar uma data a partir de uma string significa converter o texto que representa uma data em um tipo de dado específico de data que o programa pode entender e manipular. Programadores fazem isso porque frequentemente lidam com datas em formatos diferentes, como entrada de usuário ou dados de arquivo, e precisam padronizar para processamento e comparação.

## Como Fazer:
Para analisar uma data em Rust, você pode usar o crate `chrono`, que é uma biblioteca de processamento de datas e horas.

```Rust
extern crate chrono;
use chrono::prelude::*;

fn main() {
    // Exemplo usando a data em formato ISO 8601
    let data_string = "2023-03-14T16:12:09Z";
    let data: DateTime<Utc> = data_string.parse().expect("Formato de data inválido!");

    println!("{:?}", data);
    // Saída: 2023-03-14T16:12:09Z
}
```
Certifique-se de adicionar `chrono` ao seu Cargo.toml:

```toml
[dependencies]
chrono = "0.4"
```

## Aprofundando
Historicamente, o processamento de datas e horas em programação é complexo devido a fatores como fusos horários, horário de verão e formatos locais. Rust resolve isso com `chrono`, mas existem alternativas como o módulo `time`, presente na biblioteca padrão, que é mais básico e menos flexível.

O `chrono` permite analisar uma variedade de formatos e também fornece funções para formatar datas. Internamente, a biblioteca lida com muitas complexidades das operações de data e hora, como validar datas, contar anos bissextos e muito mais.

Existem outros formatos e métodos para analisar datas, e você pode definir seus próprios formatadores se estiver tratando com um formato exclusivo.

## Veja Também
- Documentação oficial do `chrono`: https://docs.rs/chrono/
- Guia do Rust para trabalhar com datas e horas: https://rust-lang-nursery.github.io/rust-cookbook/datetime.html
- Livro 'The Rust Programming Language' seção sobre crates: https://doc.rust-lang.org/book/ch14-00-more-about-cargo.html
