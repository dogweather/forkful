---
title:                "Obtendo a data atual"
html_title:           "Rust: Obtendo a data atual"
simple_title:         "Obtendo a data atual"
programming_language: "Rust"
category:             "Rust"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/rust/getting-the-current-date.md"
---

{{< edit_this_page >}}

# O que é e porquê?

Obter a data atual é uma tarefa importante para programadores em qualquer linguagem de programação, incluindo Rust. Essa função permite que programas tenham acesso à data e hora atuais e possam usá-las em tarefas específicas, como registros de atividade ou agendamento de tarefas.

## Como fazer:

Em Rust, podemos obter a data atual usando o módulo `chrono`, que lida com manipulação de datas e horas. Basta importar o módulo e usar a função `Local::now()` para obter um objeto de data e hora atual. Veja um exemplo abaixo:

```Rust
extern crate chrono;

use chrono::{Local, DateTime};

fn main() {
    let data_hora_atual: DateTime<Local> = Local::now();
    println!("A data e hora atuais são: {}", data_hora_atual);
}
```

A saída seria semelhante a: `A data e hora atuais são: 2021-09-04 12:30:00`.

## Profundando na questão:

A obtenção da data é uma tarefa comum em programação e há várias maneiras de fazê-la, dependendo da linguagem e do contexto. Em versões anteriores do Rust, era preciso usar a biblioteca padrão `time` para manipulação de datas. No entanto, com o lançamento da versão 1.0 do Rust, o módulo `chrono` se tornou a solução recomendada para essa tarefa.

Existem também alternativas ao `chrono`, como a biblioteca `time`, que ainda é suportada em versões mais recentes do Rust. No entanto, o `chrono` é considerado mais fácil de usar e possui mais recursos.

Internamente, o módulo `chrono` é baseado na biblioteca `time` e fornece uma interface mais amigável, além de garantir consistência entre diferentes plataformas. Ele também oferece suporte a intervalos de tempo muito grandes ou pequenos, o que é útil em certas aplicações.

## Veja também:

- [Documentação do módulo `chrono`](https://docs.rs/chrono/)
- [Diferenças entre o `chrono` e o `time`](https://stackoverflow.com/questions/35935220/differences-between-time-and-chrono-in-rust)