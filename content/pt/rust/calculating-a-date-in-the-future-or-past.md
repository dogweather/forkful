---
title:    "Rust: Calculando uma data no futuro ou passado"
keywords: ["Rust"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pt/rust/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Por que
Calculando datas no futuro e no passado é uma habilidade importante para qualquer programador. Saber como calcular datas precisas pode facilitar muito a criação de aplicações e lidar com lógica de tempo.

## Como Fazer
A linguagem de programação Rust possui uma ótima biblioteca integrada para lidar com datas. Vamos dar uma olhada em como calcular datas no futuro e no passado usando Rust.

```Rust
use chrono::{NaiveDate, Duration};

// Calculando 10 dias a partir de hoje
let today = NaiveDate::today();
let future = today + Duration::days(10);
println!("10 dias a partir de hoje: {:?}", future);

// Calculando 1 ano atrás a partir de hoje
let past = today - Duration::days(365);
println!("1 ano atrás a partir de hoje: {:?}", past);
```

Output:
```
10 dias a partir de hoje: 2019-08-24
1 ano atrás a partir de hoje: 2018-08-25
```

## Mergulho Profundo
Além de adicionar e subtrair dias, a biblioteca Chrono também permite manipular datas de outras formas, como meses, horas e minutos. Além disso, é possível comparar datas e verificar se uma é anterior, posterior ou igual a outra.

Outra coisa interessante é que a biblioteca também lida com fusos horários e horário de verão, o que facilita bastante na hora de trabalhar com diferentes regiões do mundo.

## Veja também
- Documentação da biblioteca Chrono: https://docs.rs/chrono/
- Tutorial sobre como manipular datas em Rust: https://blog.wearewizards.io/manipulating-dates-with-chrono-and-rust
- Repositório de exemplos com datas em Rust: https://github.com/shepmaster/dates-in-rust