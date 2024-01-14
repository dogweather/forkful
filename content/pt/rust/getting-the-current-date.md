---
title:    "Rust: Obtendo a data atual"
keywords: ["Rust"]
---

{{< edit_this_page >}}

## Por que obter a data atual em Rust?

Existem diversas razões pelas quais alguém pode precisar de obter a data atual em um programa de Rust. Pode ser para registrar informações de registros, lidar com datas limites ou simplesmente para exibir a data em um formato específico. Independentemente do motivo, é uma tarefa importante que pode ser facilmente realizada através do uso das bibliotecas internas de Rust.

## Como obter a data atual em Rust

Para obter a data atual em Rust, é necessário utilizar uma biblioteca chamada `chrono`, que fornece métodos para lidar com datas e horas. Vamos dar uma olhada em um exemplo simples de como obter a data atual e imprimi-la na tela usando a função `now()` do `chrono`.

```
use chrono::Local; // importa a biblioteca chrono

fn main() {
    let local_date = Local::now(); // obtém a data atual usando a função "now()"
    println!("A data atual é {}", local_date); // imprime a data atual na tela
}
```

Este código primeiro importa a biblioteca `chrono` e em seguida, utiliza a função `now()` para obter a data atual no formato de "dia-mês-ano hora:minuto:segundo". É importante mencionar que a função `now()` utiliza o fuso horário local do sistema. Se você quiser utilizar um fuso horário diferente, basta especificá-lo na função `now()`.

## Deep Dive: Mais informações sobre como obter a data atual em Rust

A biblioteca `chrono` fornece diversas funções e métodos para trabalhar com datas e horas, além de oferecer suporte para datas específicas e fuso horário. Se você quiser aprender mais sobre como utilizar essa biblioteca, recomendamos a leitura da documentação oficial do `chrono` e a exploração de seus diversos recursos.

## Veja também

- [Documentação do `chrono`](https://docs.rs/chrono/)
- [Tutorial de datas e horas em Rust](https://dev.to/qnighy/getting-started-with-datetime-in-rust-14ie)
- [Exemplos práticos de uso da biblioteca `chrono`](https://rust-lang-nursery.github.io/rust-cookbook/datetime.html)