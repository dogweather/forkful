---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:10:50.309025-07:00
description: 'Como fazer: #.'
lastmod: '2024-03-13T22:44:46.378096-06:00'
model: gpt-4-0125-preview
summary: '#.'
title: Obtendo a data atual
weight: 29
---

## Como fazer:


### Usando a Biblioteca Padrão do Rust
A biblioteca padrão do Rust oferece uma maneira limitada, mas rápida, de obter o horário atual, embora não diretamente a data atual em um formato calendário. Veja como fazer isso:

```rust
use std::time::{SystemTime, UNIX_EPOCH};

fn main() {
    match SystemTime::now().duration_since(UNIX_EPOCH) {
        Ok(n) => println!("Horário atual: {} segundos desde a Era Unix.", n.as_secs()),
        Err(_) => panic!("SystemTime antes da Era Unix!"),
    }
}
```

Saída:
```
Horário atual: 1615390665 segundos desde a Era Unix.
```

### Usando a Biblioteca Chrono
Para uma funcionalidade de data e hora mais abrangente, incluindo obter a data atual, você deve usar a biblioteca `chrono`. Primeiro, adicione `chrono` ao seu `Cargo.toml`:

```toml
[dependencies]
chrono = "0.4"
```

Então, você pode usar `chrono` para obter a data atual:

```rust
extern crate chrono;
use chrono::{Local, Datelike};

fn main() {
    let now = Local::now();
    println!("Data atual: {}-{}-{}", now.year(), now.month(), now.day());
}
```

Saída:
```
Data atual: 2023-4-20
```

A biblioteca `chrono` torna direto ao ponto trabalhar com datas e horários, oferecendo uma ampla gama de funcionalidades além de apenas recuperar a data atual, incluindo análise, formatação e operações aritméticas em datas e horários.
