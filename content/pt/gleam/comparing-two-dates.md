---
title:                "Comparando duas datas"
date:                  2024-01-20T17:33:00.593960-07:00
model:                 gpt-4-1106-preview
simple_title:         "Comparando duas datas"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/gleam/comparing-two-dates.md"
---

{{< edit_this_page >}}

## What & Why?
Comparar duas datas significa checar a diferença ou igualdade entre elas - útil para agendar eventos, verificar validades, ou organizar cronogramas. Programadores fazem isso para manipular e dar sentido a informações temporais em suas aplicações.

## How to:
Vamos ver como comparar datas em Gleam. Suponha que você tenha duas datas e queira saber qual é a mais recente, ou se são a mesma.

```gleam
import gleam/calendar.{Date, now, is_after, is_before, is_same}

pub fn main() {
  let date1 = Date(year: 2023, month: 3, day: 15)
  let date2 = Date(year: 2023, month: 4, day: 10)

  let today = now()

  // Comparando as datas
  io.debug(is_after(date2, date1))  // true
  io.debug(is_before(date1, date2)) // true
  io.debug(is_same(today, date1))    // Depende do dia "hoje"
}
```

## Deep Dive
Comparar datas não é novidade, mas cada linguagem tem seus métodos. Em Gleam, usamos funções padronizadas como `is_after`, `is_before`, e `is_same` para fazer comparações claras e diretas. Além disso, há alternativas como comparação de timestamps ou uso de bibliotecas externas para mais complexidade. A implementação em Gleam é segura pois impede comparações entre tipos incompatíveis sem conversões explícitas.

## See Also
Para explorar mais, dê uma olhada nestes links:
- Documentação oficial de Gleam: https://gleam.run/book/tour/
- Biblioteca `gleam/calendar`: https://hexdocs.pm/gleam_stdlib/gleam/calendar/
