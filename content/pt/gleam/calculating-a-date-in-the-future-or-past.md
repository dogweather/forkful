---
title:                "Calculando uma data no futuro ou passado"
html_title:           "Gleam: Calculando uma data no futuro ou passado"
simple_title:         "Calculando uma data no futuro ou passado"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/gleam/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

# O que & Porquê?

Calcular a data no futuro ou no passado é um processo muito comum na programação. Isso envolve determinar uma data específica com base em uma data inicial e uma duração de tempo, como um número de dias ou meses. Os programadores fazem isso para automatizar tarefas, como programar lembretes ou agendar eventos.

# Como fazer:

O Gleam tem várias funções embutidas que podem ser usadas para calcular a data no futuro ou no passado. Aqui está um exemplo de código que calcula a data de nascimento de uma pessoa daqui a 25 anos:

```Gleam
let data_nascimento = Date.from_iso8601("1997-05-12")
let data_futura = Date.add(data_nascimento, Time.Duration.months(25 * 12))
```

A saída será ```2022-05-12```, a data de nascimento da pessoa daqui a 25 anos. Você também pode adicionar ou subtrair dias, semanas, anos e até mesmo períodos de tempo mais específicos, como horas ou minutos.

# Mergulho profundo:

O cálculo de datas é uma tarefa fundamental na programação e é usado em muitas aplicações diferentes, como gerenciamento de calendário, sistemas de agendamento e processamento de transações financeiras. Além do Gleam, existem outras linguagens de programação que também têm funções embutidas para calcular datas, como Python e Java.

Ao trabalhar com datas no Gleam, é importante ter em mente os formatos de data comuns, como o ISO 8601 (YYYY-MM-DD), que é usado no exemplo acima. Também é importante entender como a linguagem manipula os tipos de dados de data e tempo e como fazer conversões entre eles.

# Veja também:

- Documentação oficial do Gleam sobre funções de data: https://gleam.run/documentation/standard-library/datetime
- Exemplo de cálculo de datas com Python: https://www.geeksforgeeks.org/python-program-to-calculate-number-of-days-between-two-given-dates/