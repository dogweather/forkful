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

## Por que

O cálculo de datas futuras ou passadas é uma tarefa frequente em muitos programas, pois permite que os usuários tenham informações precisas sobre eventos futuros ou dados históricos. Além disso, essa funcionalidade é fundamental para lidar com transações financeiras ou agendamento de tarefas.

## Como Fazer

Para calcular uma data no futuro ou passado em Rust, você pode usar a biblioteca padrão `chrono`, que fornece uma variedade de funções para manipular datas. Para isso, primeiro é necessário importar a biblioteca no seu projeto:

```rust
use chrono::{Duration, Local};
```

Em seguida, para calcular uma data futura, podemos usar a função `add()` em um objeto do tipo `NaiveDate` para adicionar uma duração específica à data atual, por exemplo:

```rust
let data_atual = Local::today().naive_local();
let data_futura = data_atual.add(Duration::days(30));
println!("A data daqui a 30 dias será: {}", data_futura);
```

O output será: `A data daqui a 30 dias será: 2021-08-12`.

Para calcular uma data passada, podemos usar a função `subtract()` da mesma forma. Por exemplo:

```rust
let data_passada = data_atual.subtract(Duration::days(15));
println!("A data há 15 dias atrás foi: {}", data_passada);
```

O output será: `A data há 15 dias atrás foi: 2021-06-18`.

## Deep Dive

O `chrono` é uma biblioteca bastante flexível e permite que você faça cálculos não apenas com dias, mas também com horas, minutos, segundos e até mesmo anos. Além disso, é possível realizar operações mais complexas, como adicionar ou subtrair diferentes durações. Para saber mais sobre todas as funções disponíveis, é recomendado consultar a documentação oficial da biblioteca.

## Veja também

- [Documentação oficial do `chrono`](https://docs.rs/chrono/0.4.19/chrono/index.html)
- [Tutorial sobre como usar a biblioteca `chrono` em Rust](https://www.steadylearner.com/blog/read/How-to-use-chrono-date-and-time-library-in-Rust)
- [Exemplos práticos de cálculos de datas com o `chrono`](https://opensource.com/article/20/6/datetime-rust)