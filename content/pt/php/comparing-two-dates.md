---
title:                "PHP: Comparando duas datas"
simple_title:         "Comparando duas datas"
programming_language: "PHP"
category:             "PHP"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/php/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Por que comparar duas datas em um programa PHP?

Comparar datas é uma tarefa comum em programação, especialmente quando se lida com dados relacionados a eventos ou cronogramas. Ao comparar duas datas, é possível determinar se uma data é anterior, posterior ou igual à outra, o que pode ser útil em diversas situações, como calcular a idade de uma pessoa ou verificar a validade de um documento. Neste artigo, vamos mostrar como comparar duas datas em PHP e aprofundar mais no assunto.

## Como comparar datas em PHP

Para comparar duas datas em PHP, usamos a função `strtotime()` para transformar as datas em um formato numérico, que pode ser facilmente comparado. Por exemplo, digamos que queremos comparar as datas de nascimento de duas pessoas diferentes. Teríamos o seguinte código:

```PHP
$primeira_data = strtotime("10 June 1990");
$segunda_data = strtotime("15 September 1988");

if ($primeira_data > $segunda_data) {
  echo "A primeira pessoa é mais nova do que a segunda.";
} elseif ($primeira_data < $segunda_data) {
  echo "A primeira pessoa é mais velha do que a segunda.";
} else {
  echo "Ambas as pessoas nasceram no mesmo dia.";
}
```

Neste exemplo, usamos a função `strtotime()` para transformar as datas em formato de timestamp, e em seguida comparamos as duas variáveis usando as condições `if`, `elseif` e `else`. O código acima resultaria em "A primeira pessoa é mais nova do que a segunda". 

## Aprofundando na comparação de datas

Além de comparar datas baseadas em valores numéricos, também podemos usar a função `date_diff()` para obter o intervalo entre duas datas. Por exemplo, se quiséssemos saber quantos dias se passaram entre duas datas específicas, poderíamos usar o seguinte código:

```PHP
$data1 = date_create("1985-02-19");
$data2 = date_create("2021-02-19");
$intervalo = date_diff($data1, $data2);

echo $intervalo->format("%a dias");
```

Neste exemplo, usamos a função `date_create()` para criar dois objetos de data e em seguida usamos a função `date_diff()` para obter o intervalo entre as duas datas. O resultado seria "13169 dias".

## Veja também

- [Documentação PHP para a função strtotime](https://www.php.net/manual/en/function.strtotime.php)
- [Documentação PHP para a função date_diff](https://www.php.net/manual/en/function.date-diff.php)