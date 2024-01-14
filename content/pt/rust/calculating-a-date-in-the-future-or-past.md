---
title:                "Rust: Calculando uma data no futuro ou passado"
programming_language: "Rust"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/rust/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Por que usar Rust para calcular datas no futuro ou passado?

Se você é um programador, provavelmente já precisou realizar cálculos com datas em algum momento. Pode ser para agendar um evento, programar uma tarefa ou simplesmente para obter informações sobre a data atual. Usar a linguagem de programação Rust pode ser uma ótima opção para esse tipo de tarefa.

## Como fazer

Para calcular uma data futura ou passada em Rust, precisamos primeiro importar a biblioteca `chrono`. Essa biblioteca nos permite trabalhar com datas e tempos de uma forma simples e eficiente.

Vamos supor que queremos calcular o dia atual daqui a duas semanas. Podemos fazer isso com poucas linhas de código em Rust:

```
use chrono::{Local, Duration};

let today = Local::today(); // Obtém a data atual
let fut_date = today + Duration::days(14); // Adiciona 14 dias à data atual
println!("{}", fut_date); // Imprime a data futura
```

O código acima irá imprimir a data exatamente daqui a duas semanas a partir de hoje. Da mesma forma, podemos calcular uma data no passado, basta mudar o operador de adição para subtração.

## Aprofundando

A biblioteca `chrono` também permite calcular datas com mais precisão, como levar em consideração horas, minutos e segundos. Além disso, podemos calcular datas em diferentes fusos horários e realizar operações como comparação entre datas.

Outra funcionalidade interessante é a capacidade de formatar a data de acordo com o idioma e região desejados. Isso pode ser especialmente útil em aplicações com usuários de diferentes partes do mundo.

Com o uso correto da biblioteca `chrono`, podemos simplificar tarefas de manipulação de datas e tornar nosso código mais legível e eficiente.

## Veja também

- Documentação da biblioteca `chrono`: https://docs.rs/chrono/0.4.19/chrono/
- Tutorial de manipulação de datas com Rust: https://erwabook.com/intro/datetime.html