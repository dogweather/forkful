---
title:                "Calculando uma data no futuro ou passado"
date:                  2024-01-20T17:31:26.791419-07:00
model:                 gpt-4-1106-preview
simple_title:         "Calculando uma data no futuro ou passado"

category:             "PHP"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/php/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## O Quê e Porquê?

Calcular uma data no futuro ou no passado é simplesmente identificar uma data além ou antes da atual. Programadores fazem isso para lidar com funcionalidades como lembretes, assinaturas, e cronogramas de eventos.

## Como Fazer:

```PHP
<?php
// Calculando uma data no futuro
$dataFutura = date('Y-m-d', strtotime('+1 week'));
echo $dataFutura; // Saída: [data da próxima semana no formato AAAA-MM-DD]

// Calculando uma data no passado
$dataPassada = date('Y-m-d', strtotime('-1 month'));
echo $dataPassada; // Saída: [data de um mês atrás no formato AAAA-MM-DD]
?>
```

## Mergulho Profundo:

Historicamente, a função `strtotime` tem sido o workhorse no PHP para manipulação de datas. Alternativas como a classe `DateTime` introduzida no PHP 5.2 oferecem mais flexibilidade e objetos mais sofisticados para manipulação de datas. Ao calcular datas no futuro ou no passado, lembre-se dos fusos horários: a classe `DateTimeZone` pode ser essencial para precisão. Outra consideração é o comportamento do PHP em relação a meses com diferentes números de dias ao somar ou subtrair meses, o que pode não ser intuitivo. Por exemplo, adicionar um mês a 31 de Janeiro não resultará em 31 de Fevereiro, mas sim em 28 ou 29 de Fevereiro, dependendo do ano.

## Veja Também:

- Manual do PHP sobre a função strtotime: https://www.php.net/manual/pt_BR/function.strtotime.php
- Manual do PHP sobre a classe DateTime: https://www.php.net/manual/pt_BR/class.datetime.php
- Artigo sobre manipulação de datas com DateTime: https://www.php.net/manual/pt_BR/datetime.format.php
- Uma discussão sobre fusos horários e PHP: https://www.php.net/manual/pt_BR/class.datetimezone.php
