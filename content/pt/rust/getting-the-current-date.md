---
title:                "Obtendo a data atual"
html_title:           "C: Obtendo a data atual"
simple_title:         "Obtendo a data atual"
programming_language: "Rust"
category:             "Rust"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/rust/getting-the-current-date.md"
---

{{< edit_this_page >}}

## O que & Por quê?

Obter a data atual em programação é a ação de recolher o momento exato em que o código está sendo executado. Isso é feito para registrar eventos, fazer marcas de tempo ou gerenciar tarefas cronometradas.

## Como Fazer:

Vamos ver como fazer isso na linguagem Rust. No bloco de código a seguir, estou usando a crate `chrono` para obter a data atual:

```Rust
use chrono::{DateTime, Local};

fn main() {
    let agora: DateTime<Local> = Local::now();
    println!("{}", agora);
}
```

Quando executado, o output será algo como:

```Rust
2023-05-09 14:10:10.123456 -03:00
```

## Deep Dive

Obter a data atual é uma prática comum e histórica na programação. Antes das bibliotecas modernas como a `chrono`, os programadores usavam estruturas de baixo nível para acessar o relógio do sistema e tratar a informação.

Em Rust, além da `chrono`, há alternativas como a `time`, que também permite obter a data e hora atuais com precisão de nanossegundos.

O detalhe de implementação principal ao obter a data atual em Rust é usar um `DateTime` parametrizado com um `TimeZone` específico. `Local::now()` retorna uma `DateTime<Local>`, que representa a data e a hora atuais no fuso horário local.

## Veja Também

Para mais informações, dê uma olhada na documentação oficial das crates `chrono` e `time`:

- `chrono`: https://docs.rs/chrono/0.4.19/chrono/
- `time`: https://docs.rs/time/0.3.5/time/