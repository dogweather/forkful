---
title:                "Gleam: Convertendo uma data em uma cadeia de caracteres"
simple_title:         "Convertendo uma data em uma cadeia de caracteres"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/gleam/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Por que Converter uma Data em String?

Converter uma data em string pode ser uma tarefa muito útil em programação, especialmente quando se trabalha com dados relacionados a datas. Isso permite que o programador tenha maior controle e flexibilidade sobre como a data será apresentada no código e em interfaces de usuário.

## Como Fazer

Para converter uma data em string no Gleam, primeiro é necessário definir a data no formato desejado. Isso pode ser feito com a função `now()` para obter a data atual ou com a função `Date.from_gregorian(year, month, day)` para criar uma data específica.

Uma vez que a data esteja definida, podemos usar a função `Date.to_string(date)` para convertê-la em uma string. Por exemplo:

```Gleam
let data = Date.now()
let data_string = Date.to_string(data)

// data_string será algo como "2021-05-20T13:45:24+00:00"
```

O formato padrão para a função `Date.to_string()` é o [ISO 8601](https://en.wikipedia.org/wiki/ISO_8601), mas é possível especificar um formato personalizado, como mostrado abaixo:

```Gleam
let data = Date.now()
let data_string = Date.to_string(data, "%d/%m/%Y")

// data_string será algo como "20/05/2021"
```

## Mergulho Profundo

Além de converter uma data em uma string em um formato específico, o Gleam também nos permite fazer operações com datas, como adicionar ou subtrair dias, semanas, meses ou anos. Isso é feito com a função `Date.add_time(date, time)` ou `Date.sub_time(date, time)`, onde `time` é uma struct do tipo `Time` que especifica a quantidade de tempo que deve ser adicionada ou subtraída.

## Veja também

- [Documentação Gleam sobre datas](https://gleam.run/documentation/stdlib/date.html)
- [Tutorial sobre manipulação de datas com Gleam](https://dreamsimilia.com/gleam-date-manipulation/)