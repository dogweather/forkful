---
title:    "Rust: Comparando duas datas"
keywords: ["Rust"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pt/rust/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Por que comparar duas datas em Rust é importante

Comparar datas é uma tarefa comum e essencial em muitos programas de computador. Ao comparar duas datas em Rust, podemos verificar se uma data é anterior, posterior ou igual a outra. Isso pode ser útil em uma variedade de cenários, como gerenciamento de eventos, controle de versão de arquivos ou análise de dados temporais. Nesta postagem do blog, exploraremos como comparar duas datas em Rust e exibiremos exemplos práticos.

## Como comparar duas datas em Rust

Em Rust, podemos comparar duas datas por meio do operador `==` ou dos métodos `eq()`, `ne()`, `lt()`, `gt()` e `cmp()` da estrutura `DateTime` da biblioteca `chrono`. Vejamos um exemplo de comparação de datas usando o operador `==`:

```rust
use chrono::{Date, Datelike, Utc};

let date1 = Utc::today();
let date2 = Date::from_ymd(2022, 1, 1);

if date1 == date2 {
    println!("As duas datas são iguais!");
} else {
    println!("As datas são diferentes!");
}
```

Este código criará duas datas, uma referente ao dia atual e outra ao dia 1 de janeiro de 2022. Em seguida, o operador `==` é utilizado para compará-las e, dependendo do resultado, uma mensagem será exibida.

Outros métodos também podem ser usados, como por exemplo `lt()`, que verifica se a primeira data é menor que a segunda, ou `ne()`, que verifica se elas são diferentes. Além disso, o método `cmp()` retorna um resultado `Ordering` baseado na comparação entre as datas, que pode ser usado em estruturas de controle de fluxo.

## Mergulhando mais fundo

Ao comparar duas datas em Rust, é importante entender como esses métodos funcionam e como as datas são representadas. Internamente, as datas são armazenadas como a quantidade de segundos a partir de uma data de referência, conhecida como "época". Em Rust, a época é definida como sendo o dia 1º de janeiro de 1970 às 00:00:00 UTC. Isso significa que podemos comparar datas de diferentes fusos horários sem nos preocuparmos com a diferença de horário.

Além disso, é importante notar que, ao criar uma data sem especificar um fuso horário, o padrão é ser utilizada a hora UTC.

## Veja também

- [Documentação da biblioteca `chrono`](https://docs.rs/chrono/latest/chrono/)
- [Tutorial sobre manipulação de datas em Rust](https://dev.to/adam_orlowsky/how-to-manipulate-dates-and-times-in-rust-4g8m)