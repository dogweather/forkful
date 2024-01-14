---
title:                "Rust: Obtendo a data atual"
programming_language: "Rust"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/rust/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Por que obter a data atual em Rust?

Existem várias situações em que você pode precisar obter a data atual em seu programa Rust. Por exemplo, aplicativos de gerenciamento de tarefas ou de calendário precisam mostrar a data atual ou executar tarefas com base nela. Obter a data atual também pode ser útil para rastrear eventos ou para gerar logs diários.

## Como obter a data atual em Rust

Há várias maneiras de obter a data atual em Rust. Uma das maneiras mais comuns é usar a biblioteca `chrono`, que fornece funções para trabalhar com datas e horários. Veja um exemplo de como usar a biblioteca `chrono` para obter a data atual:

```Rust
use chrono::{Local, Datelike};
let today = Local::now().date();
println!("Data atual: {}-{}-{}", today.day(), today.month(), today.year());
```

Este código primeiro importa a estrutura de data e horário `Local` e o método `Datelike` da biblioteca `chrono`. Em seguida, usa o método `now()` para obter a data e hora atual e o método `date()` para obter somente a data atual. Por fim, a data é impressa no formato "dia-mês-ano".

## Aprofundando

Por trás dos panos, a biblioteca `chrono` está usando o sistema operacional para obter a data atual. Isso significa que a data atual pode ser afetada por configurações de fuso horário ou outras configurações do sistema. Além disso, se você precisar de uma precisão maior, também pode usar a estrutura `UTC` em vez de `Local` para obter a hora universal coordenada.

Outra coisa importante a observar é que as datas são mutáveis ​​em Rust, pois a biblioteca `chrono` não faz distinção entre data e hora. Portanto, tome cuidado ao manipular as datas e sempre verifique se você está trabalhando com o tipo de dado correto.

## Veja também

- [Documentação da biblioteca chrono](https://docs.rs/chrono/0.4.19/chrono/)
- [Outras bibliotecas de data e hora em Rust](https://lib.rs/datetime)