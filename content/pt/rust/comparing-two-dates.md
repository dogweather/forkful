---
title:                "Rust: Comparando duas datas"
simple_title:         "Comparando duas datas"
programming_language: "Rust"
category:             "Rust"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/rust/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Por que comparar duas datas em Rust?

Comparar datas é uma tarefa comum em muitas linguagens de programação, inclusive em Rust. É importante saber como fazer essa comparação corretamente para garantir que seu código funcione corretamente e produza os resultados esperados.

## Como fazer a comparação de datas em Rust

Para comparar duas datas em Rust, podemos utilizar o módulo `chrono`. Primeiro, precisamos importar esse módulo no nosso código.

```
use chrono::prelude::*;
```

Em seguida, podemos criar duas variáveis do tipo `DateTime` e especificar as datas que queremos comparar.

```
let data1 = Utc.ymd(2021, 1, 1).and_hms(0, 0, 0);
let data2 = Utc.ymd(2020, 1, 1).and_hms(12, 0, 0);
```

Para realizar a comparação, podemos utilizar os operadores `==`, `!=`, `>`, `<`, `>=` e `<=`.

```
println!("A data1 é igual à data2? {}", data1 == data2);
println!("A data1 é diferente da data2? {}", data1 != data2);
println!("A data1 é maior que a data2? {}", data1 > data2);
println!("A data1 é menor que a data2? {}", data1 < data2);
println!("A data1 é maior ou igual à data2? {}", data1 >= data2);
println!("A data1 é menor ou igual à data2? {}", data1 <= data2);
```

Ao executar esse código, obteremos o seguinte resultado:

```
A data1 é igual à data2? false
A data1 é diferente da data2? true
A data1 é maior que a data2? true
A data1 é menor que a data2? false
A data1 é maior ou igual à data2? true
A data1 é menor ou igual à data2? false
```

## Aprofundando-se na comparação de datas em Rust

Ao comparar datas em Rust, é importante levar em conta que o tipo `DateTime` armazena informações de data e hora, incluindo o fuso horário. Isso significa que se as datas estiverem em fusos horários diferentes, a comparação será baseada nessa diferença de horários.

Além disso, é importante ficar atento à formatação das datas. Se as datas estiverem em formatos diferentes, a comparação pode não funcionar corretamente. Portanto, certifique-se de que as datas estejam em um formato padrão antes de realizar a comparação.

## Veja também

- Documentação oficial do módulo `chrono`: https://docs.rs/chrono/
- Tutorial sobre como comparar datas em Rust: https://www.joshmcguigan.com/blog/comparison-operators-for-rusts-datetime/