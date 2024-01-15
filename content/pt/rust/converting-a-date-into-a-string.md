---
title:                "Convertendo uma data em uma string"
html_title:           "Rust: Convertendo uma data em uma string"
simple_title:         "Convertendo uma data em uma string"
programming_language: "Rust"
category:             "Rust"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/rust/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Por que

Convertendo uma data em uma string é uma tarefa comum ao trabalhar com datas em um código Rust. É importante saber como fazer isso para poder manipular e apresentar datas de forma eficiente e legível.

## Como fazer

Para converter uma data em uma string, precisamos utilizar o módulo "time" do Rust e sua função ".strftime()". A sintaxe é a seguinte:

```Rust
use time::{Date,  Time, OffsetDateTime, strftime};
let data = Date::from_calendar_date(2021, 10, 23)?; //data de exemplo
let string_data = data.strftime("%Y-%m-%d")?; //formato desejado
```

Neste exemplo, criamos uma data de exemplo com o ano, mês e dia desejados e, em seguida, utilizamos a função .strftime() para convertê-la em uma string, especificando o formato desejado dentro das aspas. Existem vários formatos disponíveis, como "%d/%m/%Y" para apresentar a data no formato dd/mm/aaaa. O "?" após a função indica que, em caso de erro, o programa irá retornar uma mensagem de erro ao invés de quebrar.

Agora, vamos imprimir essa string na tela. Para isso, utilizamos o comando "println!" dentro de um "match":

```Rust
match string_data {
    Ok(_s) => {
        println!("A data convertida é: {}", _s);
    }
    Err(e) => {
        println!("Erro: {}", e);
    }
}
```

Caso não haja erro na conversão, a string convertida será impressa na tela. Caso contrário, será impressa uma mensagem de erro. O underline antes da variável "s" significa que não estamos utilizando ela, mas precisamos dela no código para que o "match" funcione. Por isso, o símbolo "_" pode ser utilizado para indicar variáveis que não serão utilizadas.

O resultado desta implementação será: "A data convertida é: 2021-10-23".

## Deep Dive

O módulo "time" do Rust oferece muitas opções para formatar uma data em uma string. Além disso, ele também permite manipular datas, adicionar ou subtrair dias, comparar datas, entre outras funcionalidades.

Algumas das opções de formatação disponíveis são:

- "%A": dia da semana completo (e.g.: "terça-feira")
- "%a": dia da semana abreviado (e.g.: "ter")
- "%B": mês completo (e.g.: "outubro")
- "%b": mês abreviado (e.g.: "out")
- "%Y": ano com 4 dígitos (e.g.: "2021")
- "%y": ano com 2 dígitos (e.g.: "21")
- "%m": mês com 2 dígitos (e.g.: "10" para outubro)
- "%d": dia com 2 dígitos (e.g.: "23")

Além disso, também é possível utilizar símbolos para separar os valores dentro da string, como o "-" ou "/".

## Veja também

Para mais informações sobre o módulo "time" do Rust e suas funcionalidades, consulte a documentação oficial:

- https://docs.rs/time/
- https://doc.rust-lang.org/std/time/
- https://rust-lang.github.io/rustc-serialize/serialize/time/