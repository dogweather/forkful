---
title:                "Obtendo a data atual"
date:                  2024-01-20T15:16:26.369794-07:00
simple_title:         "Obtendo a data atual"

tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/rust/getting-the-current-date.md"
---

{{< edit_this_page >}}

## O quê & Por quê?
Descobrir a data atual é uma questão de pedir ao sistema que te diga que dia é hoje. Programadores fazem isso para logs, timestamps, ou simplesmente para mostrar a data em um app.

## Como Fazer:
```Rust
use chrono::{Local, Datelike};

fn main() {
    let hoje = Local::today();
    println!("Data de hoje: {}", hoje);
    // Saida: Data de hoje: AAAA-MM-DD 
}
```

```Rust
use std::time::{SystemTime, UNIX_EPOCH};
use chrono::{DateTime, Utc};

fn main() {
    let agora = SystemTime::now();
    let desde_epoch = agora.duration_since(UNIX_EPOCH)
        .expect("Time went backwards");
    let datetime: DateTime<Utc> = DateTime::from(agora);
    println!("Data e hora atuais UTC: {}", datetime);
    // Saida: Data e hora atuais UTC: AAAA-MM-DDTHH:MM:SS.ZZZZZZ
}
```

## Mergulho Profundo
Antes, Rust tinha funcionalidades de tempo e data no `std::time`, mas isso mudou. Hoje, a biblioteca `chrono` é o padrão de facto para lidar com data e hora em Rust, oferecendo precisão e facilidade de uso. Há alternativas como `time` e `date`, mas `chrono` é mais popular.

Implementar a obtenção da data atual pode variar dependendo do sistema operacional e do fuso horário. `chrono` lida com essas nuances, permitindo a conversão entre fusos horários e sistemas de calendário.

## Veja Também
- Documentação do `chrono`: [docs.rs/chrono](https://docs.rs/chrono/)
- Guia de tempo e data no Rust Cookbook: [rust-lang-nursery.github.io/rust-cookbook](https://rust-lang-nursery.github.io/rust-cookbook/)
- Módulo de tempo do std: [doc.rust-lang.org/std/time](https://doc.rust-lang.org/std/time/)
