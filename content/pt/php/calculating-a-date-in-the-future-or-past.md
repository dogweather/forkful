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

## Por que

Às vezes, precisamos calcular uma data no futuro ou no passado em nosso código PHP. Isso pode ser útil em diversas situações, como agendamento de tarefas, cálculo de prazos ou simplesmente para exibir datas em diferentes formatos.

## Como Fazer

Para calcular uma data no futuro ou no passado em PHP, podemos usar a função `date()` juntamente com a função `strtotime()`. A função `date()` nos permite formatar a data de acordo com nossas necessidades e a função `strtotime()` nos permite realizar cálculos com datas. Veja um exemplo:

```PHP
<?php 
// calcular a data de 5 dias a partir de hoje
$data = date('Y-m-d', strtotime('+5 days'));
echo $data; // output: 2020-11-08
```

Neste exemplo, usamos a letra `Y` para representar o ano, `m` para representar o mês e `d` para representar o dia na função `date()`. Além disso, usamos `+5 days` como parâmetro para a função `strtotime()`, que significa que queremos adicionar 5 dias à data atual. Mas e se quisermos subtrair dias ou até mesmo calcular meses ou anos inteiros? Podemos fazer isso alterando o parâmetro da função `strtotime()`:

```PHP
<?php 
// calcular a data de 2 semanas atrás
$data = date('Y-m-d', strtotime('-2 weeks'));
echo $data; // output: 2020-10-25

// calcular a data de 3 meses no futuro
$data = date('Y-m-d', strtotime('+3 months'));
echo $data; // output: 2021-02-05

// calcular a data de 1 ano e meio no futuro
$data = date('Y-m-d', strtotime('+1 year +6 months'));
echo $data; // output: 2022-05-04
```

E se quisermos calcular uma data específica, por exemplo, o dia dos pais deste ano? Podemos fazer isso informando a data desejada no segundo parâmetro da função `strtotime()`:

```PHP
<?php 
// calcular a data do dia dos pais deste ano
$data = date('Y-m-d', strtotime('second Sunday of September 2020'));
echo $data; // output: 2020-09-13
```

Além disso, se quisermos exibir a data em um formato diferente, podemos fazer isso alterando o primeiro parâmetro da função `date()`. Por exemplo, se quisermos exibir a data com o nome do mês por extenso, podemos usar a letra `F`:

```PHP
<?php 
// calcular a data do dia dos pais deste ano
$data = date('d \d\e F \d\e Y', strtotime('second Sunday of September 2020'));
echo $data; // output: 13 de setembro de 2020
```

## Mergulho Profundo

Além dos exemplos apresentados acima, a função `strtotime()` também nos permite realizar cálculos mais complexos com datas, como adicionar ou subtrair anos, semanas, dias e até mesmo horas, minutos e segundos. Além disso, podemos combinar diferentes funções de data do PHP para obter o resultado desejado.

Também é importante lembrar que a função `strtotime()` pode aceitar diferentes formatos de datas, como `YYYY/MM/DD` ou `MM/DD/YYYY`, portanto, é importante verificar a documentação oficial do PHP para mais informações e exemplos.

## Veja Também

- [Função date() no PHP.net](https://www.php.net/manual/pt_BR/function.date.php)
- [Função strtotime() no PHP.net](https://www.php.net/manual/pt_BR/function.strtotime.php)
- [Documentação completa de funções de data no PHP.net](https://www.php.net/manual/pt_BR/datetime.formats.php)