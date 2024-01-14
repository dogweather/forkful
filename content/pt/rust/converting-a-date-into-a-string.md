---
title:                "Rust: Convertendo uma data em uma string"
simple_title:         "Convertendo uma data em uma string"
programming_language: "Rust"
category:             "Rust"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/rust/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Por que converter uma data em uma string?

Uma das tarefas mais comuns em programação de software é lidar com datas e horas. Às vezes, é necessário salvar essas informações em um formato de string legível para os usuários ou armazená-las em um banco de dados. Portanto, saber como converter uma data em uma string é uma habilidade fundamental para qualquer desenvolvedor.

## Como fazer em Rust

Em Rust, a biblioteca padrão fornece o módulo `chrono` para lidar com datas e horas. Para converter uma data em uma string, você precisa first importar o módulo `chrono` para o seu projeto. Em seguida, você pode usar o método `format` em um objeto `DateTime` para especificar o formato da string desejado.

```
use chrono::prelude::*;

fn main() {
    // Obtém a data e hora atual
    let now = Local::now();

    // Converte a data em uma string no formato "dia/mês/ano"
    let formatted_date = now.format("%d/%m/%Y").to_string();
    println!("Data atual: {}", formatted_date);
}
```

Este código irá produzir a seguinte saída:

```
Data atual: 03/04/2021
```

Outro formato comum é "mês/dia/ano". Para isso, basta alterar a string de formato para `"%m/%d/%Y"`.

## Mergulho Profundo

A biblioteca `chrono` em Rust possui uma ampla gama de métodos para formatar datas e horas de maneira precisa. Além dos formatos de data mostrados acima, você também pode especificar o horário e a diferença do fuso horário. Além disso, o módulo `chrono` também suporta a internacionalização, permitindo que você exiba datas em diferentes idiomas.

No entanto, é importante lembrar de sempre tratar as datas e horas com cuidado em seu código, pois podem haver diferenças entre os sistemas operacionais e configurações de fuso horário.

## Veja também

Aqui estão alguns recursos úteis para aprender mais sobre como trabalhar com datas e horas em Rust:

- [Documentação oficial do módulo chrono](https://docs.rs/chrono/0.4.19/chrono/)
- [Tutorial sobre manipulação de datas e horas em Rust](https://dev.to/davidedelpapa/managing-dates-and-times-in-rust--1l3l)
- [Artigo sobre o módulo chrono e internacionalização](https://medium.com/@shaunmitchell_42620/working-with-dates-in-rust-using-chrono-and-chrono-tz-5664e17950a2)