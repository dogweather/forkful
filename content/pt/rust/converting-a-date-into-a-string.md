---
title:                "Convertendo uma data em uma string"
html_title:           "C++: Convertendo uma data em uma string"
simple_title:         "Convertendo uma data em uma string"
programming_language: "Rust"
category:             "Rust"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/rust/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

# Convertendo uma Data para String em Rust  

## O Que & Por Quê?

Converter uma data para string em Rust permite que a informação temporal se torne manipulável como texto, facilitando operações como exibição e comparação. Programadores fazem isso para aumentar a flexibilidade e controle dos dados.

## Como fazer:

Veja abaixo um exemplo de como converter uma data para string em Rust:

```Rust
use chrono::{Local, DateTime, Datelike, Timelike};
   
fn main() {
   let agora: DateTime<Local> = Local::now();
   println!("{}", agora.to_string());
}
```

Ao executar este programa, o output será similar a:

```
2022-09-22 08:15:48.007681300 -03:00
```

## Deep Dive

Rust, uma linguagem desenvolvida em 2010, sempre colocou grande ênfase em eficiência e segurança segura. A necessidade de conversão de datas para strings veio com o objetivo de facilitar operações comuns, tais como comparação e exibição.

Existem várias maneiras de converter uma data para string em Rust, mas o método `to_string()` é o mais direto. Além disso, outras bibliotecas como a serde_json podem ser usadas para lidar com datas no formato JSON.

É importante mencionar que a conversão não altera a data original. Ou seja, você ainda possui a versão original caso precise dela em seu formato nativo.

## Veja Mais

Para mais detalhes e operações com datas e horários em Rust, confira:

[respostas na StackOverflow](https://stackoverflow.com/questions/37566935/how-to-get-current-date-time-with-chrono-in-rust?)

[Documentação do Chrono](https://docs.rs/chrono/0.4.0/chrono/format/strftime/index.html)

[Tutorial oficial Rust](https://doc.rust-lang.org/book/ch02-00-guessing-game-tutorial.html)