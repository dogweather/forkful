---
title:                "Rust: Convertendo uma data em uma string"
programming_language: "Rust"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/rust/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Por que converter uma data em uma string?

Converter uma data em uma string é uma tarefa comum que pode ser encontrada em muitos projetos de programação. É importante para os programadores serem capazes de entender como essa conversão funciona e como implementá-la corretamente em seus próprios programas escritos em Rust.

## Como fazer a conversão de uma data para uma string em Rust

Para converter uma data em uma string em Rust, podemos usar a função `format!()` e especificar o formato desejado através de uma string de formatação. Por exemplo, para obter a data atual formatada como "DD/MM/AAAA", podemos fazer o seguinte:

```Rust
use std::time::{SystemTime, UNIX_EPOCH};

let data_atual = SystemTime::now(); // Obtém a data atual
let data_em_segundos = data_atual.duration_since(UNIX_EPOCH).expect("Erro ao converter para segundos"); // Converte a data em segundos

// Usa a função format!() para formatar a data em uma string
let data_formatada = format!("{:0>2}/{}/{}", data_em_segundos.day(), data_em_segundos.month(), data_em_segundos.year());

// Imprime a data formatada
println!("{}", data_formatada); // Saída: 07/09/2021
```

## Explorando mais a fundo a conversão de data para string em Rust

A função `format!()` é bastante poderosa e permite que os programadores especifiquem vários formatos e opções de formatação para diferentes tipos de dados, incluindo datas. É importante consultar a documentação oficial do Rust para entender completamente como essa função funciona e quais formatos estão disponíveis.

Além disso, é importante considerar o uso do tipo de dados `chrono`, que é uma biblioteca de data/hora de terceiros que pode facilitar a conversão de datas e fornece mais opções de formatação do que a função `format!()` padrão.

## Veja também

- [Documentação oficial do Rust sobre a função `format!()`](https://doc.rust-lang.org/std/macro.format.html)
- [Documentação do tipo de dados `chrono`](https://docs.rs/chrono/0.4.19/chrono/)
- [Tutorial sobre como converter uma data em uma string em Rust](https://www.techiediaries.com/rust-time-date-convert-string-example/) (em inglês)