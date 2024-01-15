---
title:                "Convertendo uma data em uma string"
html_title:           "Gleam: Convertendo uma data em uma string"
simple_title:         "Convertendo uma data em uma string"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/gleam/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Por que

Há muitos casos em que precisamos converter uma data em uma string, seja para exibição ao usuário ou para armazenamento em um banco de dados. Com a ajuda de Gleam, é possível realizar essa conversão de forma rápida e eficiente. Neste artigo, vamos descobrir como fazer isso!

## Como fazer

Em Gleam, o módulo "Date" fornece várias funções úteis para trabalhar com datas. Para converter uma data em uma string, usamos a função "to_string" deste módulo. Veja um exemplo:

```Gleam
import Date
import Gleam/Sequins.String as String

let my_date = Date.from_calendar_date(2021, 8, 23)
let my_string = Date.to_string(my_date)

String.print(my_string) // "2021-08-23"
```

Neste exemplo, usamos a função "from_calendar_date" para criar uma data representando o dia 23 de agosto de 2021. Em seguida, usamos a função "to_string" para converter essa data em uma string no formato "yyyy-MM-dd". Você também pode fornecer um argumento opcional para especificar o formato da string desejada. Por exemplo:

```Gleam
import Date
import Gleam/Sequins.String as String

let my_date = Date.from_calendar_date(2021, 8, 23)
let my_string = Date.to_string(my_date, "%d/%m/%Y")

String.print(my_string) // "23/08/2021"
```

Existem vários formatos disponíveis para serem usados com a função "to_string", o que torna essa conversão muito flexível e adapta-se às necessidades do seu projeto.

## Deep Dive

Ao usar a função "to_string", é importante entender que ela retorna um tipo "FlexibleDateTime", que representa uma data e hora, e não apenas uma data. Portanto, se você passar para a função uma data com uma hora diferente de "meia-noite", a string resultante conterá essa informação. Por exemplo:

```Gleam
import Date
import Gleam/Sequins.String as String

let my_date = Date.from_calendar_date(2021, 8, 23)

// data às 15:30
let my_flex_date = Date.from_full_rfc3339("2021-08-23T15:30:00+00:00")

let my_string = Date.to_string(my_flex_date)

String.print(my_string) // "2021-08-23T15:30:00+00:00"
```

Se você quiser apenas a data em formato de string, sem a hora, você pode usar a função "from_date" para converter a data em um tipo "Date" antes de chamar a função "to_string". Por exemplo:

```Gleam
import Date
import Gleam/Sequins.String as String

let my_date = Date.from_calendar_date(2021, 8, 23)

// data às 15:30
let my_flex_date = Date.from_full_rfc3339("2021-08-23T15:30:00+00:00")

let my_string = my_flex_date
    |> Date.from_date
    |> Date.to_string

String.print(my_string) // "2021-08-23"
```

Com isso, podemos converter uma data em uma string e ainda ter controle sobre o formato e os detalhes incluídos na string resultante.

## Veja também

- Documentação oficial sobre o módulo "Date" em Gleam: https://gleam.run/modules/date.html
- Tutorial sobre uso do módulo "Date" em Gleam: https://gleam.run/tutorials/dates.html