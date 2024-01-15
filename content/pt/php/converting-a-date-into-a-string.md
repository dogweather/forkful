---
title:                "Convertendo uma data em uma string"
html_title:           "PHP: Convertendo uma data em uma string"
simple_title:         "Convertendo uma data em uma string"
programming_language: "PHP"
category:             "PHP"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/php/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Por que converter uma data em uma string?

Quando trabalhamos com datas em projetos de programação, muitas vezes precisamos exibir essas informações de uma forma mais amigável para o usuário final. Converter uma data em uma string é uma maneira de formatar e personalizar a exibição dessa informação, tornando-a mais legível e compreensível.

## Como fazer

Para converter uma data em uma string em PHP, utilizamos a função `date()`, seguida de um formato de data específico. Por exemplo:

```PHP
$data = date("d/m/Y"); // Output: 10/12/2020
```

Também é possível utilizar a função `strtotime()`, que transforma uma representação textual de data em um timestamp, e então utilizar a função `date()` para personalizar o formato da string que será exibida. Por exemplo:

```PHP
$data = strtotime("22 February 2020");
$data_formatada = date("d/m/Y", $data); // Output: 22/02/2020
```

Além disso, é possível adicionar outras formatações à string, como nome do dia da semana, mês escrito por extenso, entre outras opções. Por exemplo:

```PHP
$data = date("l, d F Y"); // Output: Thursday, 10 December 2020
```

## Deep Dive

Em algumas situações, pode ser necessário trabalhar com diferentes fusos horários ou lidar com formatos de data específicos, como no caso de datas internacionais. Para isso, é importante conhecer o funcionamento da função `date()` e suas opções de formatação, que podem ser consultadas na [documentação oficial do PHP](https://www.php.net/manual/pt_BR/function.date.php).

Além disso, é importante estar atento aos diferentes formatos de data e hora utilizados em outros países, para garantir a correta exibição e compreensão por parte dos usuários. Uma boa prática é utilizar o [Internationalization Extension (Intl)](https://www.php.net/manual/pt_BR/intl.installation.php), que permite a formatação de datas em diferentes idiomas e regiões.

## Veja também

- [Documentação da função date() em PHP](https://www.php.net/manual/pt_BR/function.date.php)
- [Documentação da função strtotime() em PHP](https://www.php.net/manual/pt_BR/function.strtotime.php)
- [Internationalization Extension (Intl) em PHP](https://www.php.net/manual/pt_BR/intl.installation.php)