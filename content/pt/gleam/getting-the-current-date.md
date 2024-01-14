---
title:                "Gleam: Obtendo a data atual."
simple_title:         "Obtendo a data atual."
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/gleam/getting-the-current-date.md"
---

{{< edit_this_page >}}

# Por que utilizar o Gleam para obter a data atual?

Quando se trata de programação, há muitas tarefas comuns que precisam ser realizadas repetidamente. Uma dessas tarefas é obter a data atual. A utilização do Gleam pode facilitar essa tarefa e economizar tempo e esforço. Vamos ver como fazer isso!

## Como fazer

Para obter a data atual utilizando o Gleam, podemos usar a função `Date.now()` que retorna o número de milissegundos desde 1º de janeiro de 1970. Podemos então converter esse valor em formato de data e hora usando a função `Date.from_milliseconds()`.

```Gleam
let milliseconds = Date.now()
let current_date = Date.from_milliseconds(milliseconds)

IO.println("Data atual:", Date.format(current_date, "%d/%m/%Y"))
IO.println("Hora atual:", Date.format(current_date, "%H:%M:%S"))
```

O código acima irá imprimir a data e hora atual no formato "dd/mm/yyyy" e "hh:mm:ss", respectivamente.

## Mergulho Profundo

Como mencionado anteriormente, `Date.now()` retorna o número de milissegundos desde 1º de janeiro de 1970. Isso é conhecido como um "timestamp" e é uma forma comum de representar datas em programação. No Gleam, podemos trabalhar facilmente com timestamps usando a estrutura `Date`.

A estrutura `Date` possui vários campos úteis, como `year`, `month`, `day`, `hour`, `minute` e `second`, que podem ser acessados individualmente. Além disso, também há funções para comparar datas, adicionar ou subtrair dias, horas, minutos ou segundos e muito mais.

Usar a estrutura `Date` ao invés de simplesmente converter o timestamp em data e hora torna o código mais legível e permite mais flexibilidade ao trabalhar com datas.

# Veja também

- Documentação oficial do Gleam: https://gleam.run/documentation/
- Gleam no GitHub: https://github.com/gleam-lang/gleam