---
title:                "Comparando duas datas"
html_title:           "C#: Comparando duas datas"
simple_title:         "Comparando duas datas"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/gleam/comparing-two-dates.md"
---

{{< edit_this_page >}}

## O Que & Porquê?

Comparar duas datas envolve a análise das mesmas para identificar qual é mais recente, mais antiga, ou se são iguais. Os programadores fazem isto para ordenar eventos, fazer cálculos de tempo ou para ativar/desativar funcionalidades com base na data.

## Como fazer:

Vamos ver como comparar duas datas em Gleam. 

```Gleam
import gleam/datetime.{Date}
import gleam/int

fn compare_dates(date1: Date, date2: Date) -> String {
  let comparison = date1.compare(date2)

  case comparison {
    Ok(Equal) -> "As datas são iguais."
    Ok(Less) -> "A primeira data é mais antiga."
    Ok(Greater) -> "A primeira data é mais recente."
    Error(e) -> e
  }
}

let date1 = Date.new(2020, 12, 1)
let date2 = Date.new(2021, 12, 1)

let result = compare_dates(date1, date2)
```

Com este exemplo, o output será "A primeira data é mais antiga."

## Pormenorização:

A comparação de datas tem sido uma prática desde o início da programação. Alternativamente, você pode usar bibliotecas de terceiros para comparar datas ou até cálculos manuais com timestamps, mas Gleam fornece uma maneira fácil de fazer isso com a função `compare`.

Leve em conta que `Date.new/3` pode lançar um erro. Assim, é recomendado usar com a função `try` para lidar com isso. Neste exemplo, simplificamos para manter a simplicidade.

## Veja também:
