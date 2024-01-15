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

## Por que
Você já se perguntou como comparar duas datas em PHP? Às vezes, precisamos verificar se uma data é maior ou menor que outra, por exemplo, para validar inputs de formulários ou classificar dados em ordem cronológica. Saber como comparar datas é uma habilidade valiosa para qualquer programador PHP.

## Como Fazer
Comparar datas em PHP é mais simples do que parece. Vamos dar uma olhada em alguns exemplos de como realizar essa tarefa.

#### Comparando duas datas
```PHP
$date1 = '2020-12-31';
$date2 = '2021-01-15';

if ($date1 < $date2) {
    echo 'A primeira data é anterior à segunda data.';
} elseif ($date1 > $date2) {
    echo 'A primeira data é posterior à segunda data.';
} else {
    echo 'As duas datas são iguais.';
}
```
Saída: A primeira data é anterior à segunda data.

#### Convertendo strings em objetos DateTime
Antes de comparar datas em PHP, é necessário converter as strings em objetos DateTime para que possamos usar métodos e propriedades específicas dessa classe.
```PHP
$date1 = '2020-12-31';
$date2 = '2021-01-15';

$date1_obj = new DateTime($date1);
$date2_obj = new DateTime($date2);

if ($date1_obj < $date2_obj) {
    echo 'A primeira data é anterior à segunda data.';
} elseif ($date1_obj > $date2_obj) {
    echo 'A primeira data é posterior à segunda data.';
} else {
    echo 'As duas datas são iguais.';
}
```
Saída: A primeira data é anterior à segunda data.

#### Usando o método compare() do objeto DateTime
Além de usar os operadores de comparação (<, >, =), podemos também usar o método compare() do objeto DateTime para comparar duas datas.
```PHP
$date1 = '2020-12-31';
$date2 = '2021-01-15';

$date1_obj = new DateTime($date1);
$date2_obj = new DateTime($date2);

$result = $date1_obj->compare($date2_obj);

if ($result == -1) {
    echo 'A primeira data é anterior à segunda data.';
} elseif ($result == 1) {
    echo 'A primeira data é posterior à segunda data.';
} else {
    echo 'As duas datas são iguais.';
}
```
Saída: A primeira data é anterior à segunda data.

## Deep Dive
Ao comparar datas em PHP, devemos ter em mente algumas coisas importantes. Primeiramente, é necessário garantir que as datas estejam no formato correto antes de compará-las. Muitas vezes, strings de datas podem ser fornecidas pelos usuários ou retiradas de bancos de dados, e pode haver diferenças no formato.

Além disso, devemos ter cuidado ao comparar datas de diferentes fusos horários, já que isso afetará o resultado da comparação. Nesse caso, é recomendado realizar a comparação entre os objetos DateTime já convertidos para o mesmo fuso horário.

## Veja Também
- Manual do PHP - Comparação de datas: https://www.php.net/manual/en/datetime.diff.php
- Tutorial de PHP Date: https://www.w3schools.com/php/php_date.asp
- Datas e horários em PHP: https://www.php.net/manual/en/datetime.formats.php