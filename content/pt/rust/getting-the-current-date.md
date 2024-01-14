---
title:    "Rust: Obtendo a data atual"
keywords: ["Rust"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pt/rust/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Por que obter a data atual?

Obter a data atual é uma tarefa comum em muitos programas e aplicativos, especialmente aqueles que lidam com tarefas relacionadas ao tempo. Com a linguagem de programação Rust, é possível obter a data atual de forma eficiente e precisa, usando as ferramentas e bibliotecas certas.

## Como fazer:

Para começar, é necessário importar a biblioteca [`chrono`](https://docs.rs/chrono/latest/chrono/) na seção de dependências do seu projeto. Em seguida, utilize o seguinte código em um bloco `main()` para obter a data atual:

```Rust
use chrono::{Local, Datelike};

let data_atual = Local::now();
println!("A data atual é: {} de {} de {}", data_atual.day(), data_atual.month(), data_atual.year());
```

Este código irá imprimir a data atual no formato "dia de mês de ano". Por exemplo, se hoje fosse 24 de fevereiro de 2021, o output seria: "A data atual é: 24 de 2 de 2021". Para formatar a data de maneira diferente, você pode explorar outras funções e métodos disponíveis na biblioteca `chrono`.

Além disso, a biblioteca `chrono` também permite obter informações como a hora e o fuso horário atual. Para isso, você pode utilizar as funções `hour()`, `minute()` e `timezone()`, respectivamente.

## Deep Dive:

A biblioteca `chrono` é construída em cima de outra biblioteca de data e hora chamada [`time`](https://docs.rs/time/latest/time/), que oferece uma interface mais baixo nível e flexível para lidar com datas e horas. Se você precisa de recursos mais avançados para manipulação de data e hora, pode ser interessante explorar a biblioteca `time` e suas possibilidades.

Além disso, a biblioteca `chrono` também possui suporte para formatação e parsing de datas em diferentes formatos, tornando possível trabalhar com datas vindas de diferentes fontes externas.

## Veja também:

- [Documentação oficial da biblioteca `chrono`](https://docs.rs/chrono/latest/chrono/)
- [Documentação oficial da biblioteca `time`](https://docs.rs/time/latest/time/)