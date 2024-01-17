---
title:                "Comparando duas datas"
html_title:           "PHP: Comparando duas datas"
simple_title:         "Comparando duas datas"
programming_language: "PHP"
category:             "PHP"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/php/comparing-two-dates.md"
---

{{< edit_this_page >}}

## O que & Porquê?

Comparar duas datas é uma tarefa comum na programação que envolve verificar se uma data é anterior ou posterior a outra. Os programadores costumam realizar essa tarefa para validar informações, fazer cálculos ou ordenar uma lista de eventos por data.

## Como fazer:

Para comparar duas datas em PHP, podemos usar a função `strtotime` para converter uma string de data em um timestamp e então comparar os valores numéricos resultantes. Por exemplo:

```
$date1 = strtotime("2020-01-01");
$date2 = strtotime("2019-01-01");

if ($date1 > $date2) {
  echo "A data 1 é posterior à data 2";
} else {
  echo "A data 1 é anterior à data 2";
}
```

A saída desse código será: "A data 1 é posterior à data 2".

## Mergulho Profundo:

A tarefa de comparar datas remonta aos primórdios da computação, quando as primeiras linguagens de programação foram desenvolvidas. Atualmente, existem várias maneiras de comparar datas em PHP, como usar a função `date_diff` ou comparar as datas diretamente em formato de string com `strtotime`.

Além disso, também é possível verificar se duas datas são iguais usando a função `date_equals`, introduzida no PHP 7.2.

## Veja Também:

- Documentação oficial do PHP sobre função `strtotime`: https://www.php.net/manual/pt_BR/function.strtotime.php
- Comparando Datas em PHP: https://www.tutorialrepublic.com/php-tutorial/php-date-and-time-operations.php