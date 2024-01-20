---
title:                "Calculando uma data no futuro ou no passado"
html_title:           "PHP: Calculando uma data no futuro ou no passado"
simple_title:         "Calculando uma data no futuro ou no passado"
programming_language: "PHP"
category:             "PHP"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/php/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## O quê & Por quê?

Calcular uma data no futuro ou passado é simplesmente descobrir qual será ou era a data em uma quantidade específica de dias, meses ou anos a partir de uma data conhecida. Programadores fazem isso para realizar diversas tarefas, como definição de datas de vencimento, programação de eventos futuros, entre outros.

## Como fazer:

Aqui está um exemplo de como você pode calcular uma data futura usando PHP:

```PHP
<?php
$data = new DateTime(); // obtém a data/hora atual
$data->add(new DateInterval('P10D')); // adiciona 10 dias
echo $data->format('Y-m-d'); // exibe a data no formato AAAA-MM-DD
?>
```

E aqui está um exemplo de como você pode calcular uma data no passado:

```PHP
<?php
$data = new DateTime(); // obtém a data/hora atual
$data->sub(new DateInterval('P10D')); // subtrai 10 dias
echo $data->format('Y-m-d'); // exibe a data no formato AAAA-MM-DD
?>
```

## Mergulhando mais profundamente:

Calcular datas futuras e passadas é uma prática comum na programação e remonta às origens dos sistemas de computadores. As técnicas específicas podem variar, mas o conceito fundamental é o mesmo.

Em PHP, alternativas para a classe DateTime incluem a função `strtotime()`, que pode ser mais fácil de usar para adições e subtrações simples.

Em termos de detalhes de implementação, a classe DateTime do PHP usa internamente a biblioteca de data e hora do C para suas operações, que por sua vez é baseada em conceitos do calendário Gregoriano.

Nota: tenha cuidado ao lidar com fusos horários ao calcular datas!

## Veja também:

- Documentação oficial do PHP para a classe DateTime: [https://www.php.net/manual/en/class.datetime.php](https://www.php.net/manual/en/class.datetime.php)
- Documentação oficial do PHP para a função strtotime: [https://www.php.net/manual/en/function.strtotime.php](https://www.php.net/manual/en/function.strtotime.php)