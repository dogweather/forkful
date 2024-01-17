---
title:                "Comparando duas datas"
html_title:           "Rust: Comparando duas datas"
simple_title:         "Comparando duas datas"
programming_language: "Rust"
category:             "Rust"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/rust/comparing-two-dates.md"
---

{{< edit_this_page >}}

## O que & Por quê?

Comparar duas datas é uma tarefa comum na programação, onde precisamos verificar qual data é anterior ou posterior a outra. Isso pode ser útil, por exemplo, para criar uma lógica de agendamento de eventos ou para ordenar uma lista de tarefas por data. Os programadores fazem isso para automatizar a comparação e economizar tempo no desenvolvimento de seus projetos.

## Como fazer:

Para comparar duas datas em Rust, podemos usar o tipo de dados `NaiveDate` da biblioteca `chrono`. Primeiro, precisamos importar a biblioteca usando `use chrono::{NaiveDate, Datelike};`. Em seguida, podemos criar duas variáveis `date1` e `date2` do tipo `NaiveDate` e usá-las para comparar as datas.

```
use chrono::{NaiveDate, Datelike};

fn main() {
    let date1 = NaiveDate::from_ymd(2020, 9, 15);
    let date2 = NaiveDate::from_ymd(2020, 9, 20);

    if date1 > date2 {
        println!("A primeira data é posterior à segunda data.");
    } else if date1 < date2 {
        println!("A primeira data é anterior à segunda data.");
    } else {
        println!("As datas são iguais.");
    }
}
```

A saída seria: `A primeira data é anterior à segunda data.`

## Mergulho profundo:

Historicamente, comparar duas datas era uma tarefa complicada e tediosa em programação, pois exigia o uso de bibliotecas complexas ou a escrita de algoritmos complexos para lidar com diferentes formatos de data e tempo. Entretanto, com o surgimento de linguagens de programação modernas, como Rust, esse processo se tornou muito mais simples e eficiente.

Uma alternativa ao uso da biblioteca `chrono` é a `time`, que também possui funções para manipulação de datas e tempos em Rust. É importante lembrar que, para comparar duas datas, é preciso que elas estejam no mesmo formato, seja `UTC (Tempo Universal Coordenado)` ou `horário local`.

No nível de implementação, comparar duas datas envolve converter as datas em valores numéricos, representando-os em uma escala do tempo. Cada linguagem de programação tem sua própria forma de implementar essa conversão, mas em geral, isso é feito por meio do uso de algoritmos e coleta de informações de sistemas operacionais.

## Veja também:

- Documentação da biblioteca Chrono: https://docs.rs/chrono/0.4.19/chrono/
- Documentação da biblioteca Time: https://docs.rs/time/0.2.21/time/