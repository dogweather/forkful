---
title:    "Rust: Convertendo uma data em uma string"
keywords: ["Rust"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pt/rust/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Por que converter uma data para uma string?

Ao trabalhar com dados em programação, é comum precisar converter informações de um tipo para outro. Uma situação comum é a conversão de uma data em um formato de string legível para os usuários. Em Rust, existem várias maneiras de fazer isso, e neste artigo exploraremos algumas delas.

## Como fazer

A conversão de uma data para uma string em Rust é feita através de uma função na biblioteca padrão chamada `format!()`. Esta função permite que você formate dados em uma sequência de caracteres de acordo com um template. Por exemplo, se você tiver uma data no formato "dd/mm/aaaa" e quiser convertê-la para o formato "dd de mês de aaaa", você pode usar o seguinte código:

```Rust
use std::time::{SystemTime, UNIX_EPOCH};

let date = SystemTime::now().duration_since(UNIX_EPOCH).unwrap();
let formatted_date = format!("{:02} de {} de {:04}", date.day(), date.month(), date.year());
println!("{}",formatted_date); // saída: 26 de agosto de 2020
```

Neste exemplo, usamos a função `format!()` para criar uma string com a data atual. O primeiro argumento é um template que contém marcadores de posição para os valores a serem preenchidos. Em seguida, passamos os valores específicos para cada marcador de posição, no caso, o dia, mês e ano da data atual.

## Deep Dive

Além da função `format!()`, existem outras maneiras de converter uma data para uma string em Rust. Uma delas é usando a biblioteca `chrono`, que oferece diversos recursos para trabalhar com datas e horas. Abaixo está um exemplo de como usá-la para converter uma data para uma string:

```Rust
use chrono::{Local, Datelike};

let date = Local::now();
let formatted_date = date.format("%d/%m/%Y").to_string();
println!("{}", formatted_date); // saída: 26/08/2020
```

Neste exemplo, usamos o método `format()` da struct `DateTime` para especificar o formato desejado da string de data. Também podemos usar o método `parse_from_str()` para converter uma string em uma data, caso seja necessário.

## Veja também

- [Documentação da função format!() em Rust](https://doc.rust-lang.org/std/fmt/fn.format.html)
- [Documentação da biblioteca chrono em Rust](https://docs.rs/chrono/0.4.19/chrono/index.html)