---
title:                "Analisando uma data de uma string"
html_title:           "PHP: Analisando uma data de uma string"
simple_title:         "Analisando uma data de uma string"
programming_language: "PHP"
category:             "PHP"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/php/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## O que é e por que fazemos isso?

O processo de "parsear" ou "analisar" uma data a partir de uma string é uma tarefa comum na programação em PHP, que envolve extrair informações específicas de uma string que representa uma data. Isso permite que os programadores manipulem e utilizem as informações da data de forma mais flexível e precisa.

## Como fazer:

Para analisar uma data a partir de uma string em PHP, podemos utilizar a função `strtotime()`, que transforma a string em um timestamp (valor numérico que representa a data em segundos desde 1º de janeiro de 1970). Em seguida, podemos utilizar a função `date()` para formatar a data ao nosso gosto. Veja um exemplo abaixo:

```PHP
$date = "23/06/2020";
$timestamp = strtotime($date);
$new_date = date("Y-m-d", $timestamp);

echo $new_date; // saída: 2020-06-23
```

## Mergulho mais profundo:

Antes do lançamento do PHP 5, o processo de analisar datas em PHP era mais complexo e trabalhoso. Com a introdução da função `strtotime()`, essa tarefa se tornou mais simples e rápida. No entanto, existem também outras opções, como a classe `DateTime`, que oferece mais recursos e flexibilidade na manipulação de datas. Além disso, é importante lembrar que existem diferenças na formatação da data entre diferentes regiões e idiomas, o que pode afetar o resultado da análise da string.

## Veja também:

- Documentação oficial do PHP para a função `strtotime()` (https://www.php.net/strtotime)
- Documentação oficial do PHP para a classe `DateTime` (https://www.php.net/manual/en/class.datetime.php)
- Tutorial da DigitalOcean sobre como trabalhar com datas em PHP (https://www.digitalocean.com/community/tutorials/how-to-work-with-date-and-time-in-php)