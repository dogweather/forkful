---
title:                "Calculando uma data no futuro ou passado."
html_title:           "Rust: Calculando uma data no futuro ou passado."
simple_title:         "Calculando uma data no futuro ou passado."
programming_language: "Rust"
category:             "Rust"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/rust/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## O que & Por quê?
Calcular uma data no futuro ou no passado é uma função comum em muitos programas, especialmente em projetos relacionados a calendários e agendamentos. Os programadores usam essa funcionalidade para automatizar tarefas e garantir que as datas sejam corretamente calculadas e apresentadas aos usuários.

## Como fazer:
### Exemplo 1: Calculando uma data no futuro
```Rust
use chrono::{DateTime, Duration, Utc};

let now: DateTime<Utc> = Utc::now();
let future_date = now + Duration::days(30);

println!("A data de hoje é {}", now);
println!("Daqui a 30 dias será {}", future_date);
```
Saída:
```
A data de hoje é 2021-10-04 18:27:07.685025 UTC
Daqui a 30 dias será 2021-11-03 18:27:07.685025 UTC
```

### Exemplo 2: Calculando uma data no passado
```Rust
use chrono::{DateTime, Duration, Utc};

let now: DateTime<Utc> = Utc::now();
let past_date = now - Duration::weeks(2);

println!("A data atual é {}", now);
println!("Há duas semanas a data era {}", past_date);
```
Saída:
```
A data atual é 2021-10-04 18:27:07.685025 UTC
Há duas semanas a data era 2021-09-20 18:27:07.685025 UTC
```

## Deep Dive:
Existem várias bibliotecas disponíveis em Rust para ajudar com o cálculo de datas, incluindo a popular biblioteca chrono. No passado, muitas linguagens de programação tinham dificuldade em lidar com datas devido à variedade de formatos e sistemas de calendário em todo o mundo. No entanto, com o uso do padrão ISO 8601 para a representação de datas, tornou-se mais fácil para as linguagens de programação manipularem datas de forma consistente. Em Rust, o tipo de dados DateTime da biblioteca chrono é baseado nesse padrão e pode ser facilmente utilizado para calcular datas futuras e passadas.

## Veja também:
- Documentação da biblioteca chrono: https://docs.rs/chrono/
- Artigo sobre como lidar com datas em Rust: https://dev.to/ejrqrust/dates-and-times-in-rust-5l1
- Exemplos adicionais de cálculo de datas em Rust: https://stackoverflow.com/questions/29982598/how-do-i-add-30-days-to-a-given-date-in-rust