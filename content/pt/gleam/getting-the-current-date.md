---
title:    "Gleam: Obtendo a data atual"
keywords: ["Gleam"]
---

{{< edit_this_page >}}

## Por que obter a data atual?

Ao se envolver em programação com Gleam, é comum a necessidade de obter a data atual. Isso pode acontecer em diversos cenários, como para exibir a data em um site, registrar datas de eventos ou até mesmo para realizar cálculos com datas.

## Como fazer:

Para obter a data atual em Gleam, você pode utilizar a função `DateTime.now()`. Esta função retorna um registro contendo informações sobre a data e hora atual, como ano, mês, dia, hora, minuto, segundo e fuso horário. Veja abaixo um exemplo de código utilizando esta função:

```
Gleam DateTime.now()
```

O resultado deste código será um registro semelhante a este:

```
{year: 2021, month: 08, day: 10, hour: 14, minute: 30, second: 20, timezone: "UTC"}
```

Com esses dados, você pode trabalhar de diversas maneiras, como formatar a data em uma string específica ou realizar cálculos com as informações de data e hora.

## Mergulho mais profundo:

Quando se trata de obter a data atual em Gleam, é importante destacar que a função `DateTime.now()` retorna valores diferentes dependendo do fuso horário da máquina em que o código está sendo executado. Por exemplo, se você estiver em um fuso horário diferente de "UTC", os valores de hora, minuto e segundo serão diferentes dos mostrados no exemplo acima.

Além disso, é possível utilizar a biblioteca "gleam-chronos" para trabalhar com datas e horas de forma mais avançada em Gleam. Ela oferece funções como `DateTime.from_string()` para converter uma string em um registro de data e hora e `DateTime.to_string()` para transformar um registro em uma string formatada.

## Veja também:

- Documentação oficial do Gleam sobre [datas e horas](https://gleam.run/documentation/standard-library#date-and-time)
- Biblioteca "gleam-chronos" para trabalhar com datas e horas avançadas em Gleam: [https://github.com/gleam-lang/chronos](https://github.com/gleam-lang/chronos)