---
title:                "Calculando uma data no futuro ou passado."
html_title:           "PHP: Calculando uma data no futuro ou passado."
simple_title:         "Calculando uma data no futuro ou passado."
programming_language: "PHP"
category:             "PHP"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/php/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## O Que e Por Que?

Calcular uma data no futuro ou passado é um processo comum em programação. Isso envolve manipular datas e horas para determinar uma data específica com base em determinadas entradas. Programadores geralmente fazem isso para realizar tarefas como agendar eventos, validar informações, exibir informações em um formato específico ou criar recursos de temporização.

## Como Fazer:

Existem várias maneiras de calcular uma data no futuro ou passado em PHP. Uma maneira simples é usar a função `date()` para obter a data atual e depois manipulá-la usando as funções `strtotime()` ou `mktime()`. Por exemplo, para obter a data de 1 mês no futuro da data atual, podemos usar o seguinte código:

```PHP
$date = date("Y-m-d", strtotime("+1 month"));
echo $date;
```

Isso irá imprimir a data atual mais um mês, no formato AAAA-MM-DD. Se quisermos obter uma data específica no futuro, podemos usar a função `mktime()` da seguinte maneira:

```PHP
$date = date("Y-m-d", mktime(0, 0, 0, 12, 31, 2021));
echo $date;
```

Isso irá imprimir a data de 31 de dezembro de 2021. Além disso, também é possível usar a classe `DateTime` do PHP para realizar cálculos de data mais complexos.

## Deep Dive:

A manipulação de datas e horas tem sido um aspecto importante da programação desde o início da era dos computadores. Antes da introdução do Unix em 1970, as datas eram armazenadas como contagens de segundos a partir de uma data de referência específica. No entanto, com o surgimento do Unix, datas e horas começaram a serem armazenadas como estruturas de dados mais complexas, permitindo cálculos mais precisos e específicos.

Além disso, além das funções mencionadas acima, existem outras opções para realizar cálculos de datas em PHP, como o uso das classes `DateTime` e `DateInterval`. Essas classes oferecem uma variedade de métodos e propriedades que tornam a manipulação de datas ainda mais prática e eficiente.

## Veja Também:

- [Função date() do PHP](https://www.php.net/manual/pt_BR/function.date.php)
- [Função strtotime() do PHP](https://www.php.net/manual/pt_BR/function.strtotime.php)
- [Classe DateTime do PHP](https://www.php.net/manual/pt_BR/class.datetime.php)
- [Classe DateInterval do PHP](https://www.php.net/manual/pt_BR/class.dateinterval.php)