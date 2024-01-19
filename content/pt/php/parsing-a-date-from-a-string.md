---
title:                "Analisando uma data a partir de uma string"
html_title:           "PowerShell: Analisando uma data a partir de uma string"
simple_title:         "Analisando uma data a partir de uma string"
programming_language: "PHP"
category:             "PHP"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/php/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## O Que & Por Quê?

Analisar uma data a partir de uma string envolve a conversão de uma data no formato texto para um formato mais manipulável, como um objeto de data. Programadores fazem isso para poder calcular durações, formatar datas de modo conveniente e usar datas em outros contextos.

## Como Fazer:

Em PHP, podemos usar a função `date_create_from_format()`. Aqui está um exemplo:

```PHP
$stringDeData = "21-12-2022";
$objetoDeData = date_create_from_format('d-m-Y', $stringDeData);
echo date_format($objetoDeData, 'Y-m-d');

// Output: 2022-12-21
```

Neste exemplo, a string "21-12-2022" é analisada para um objeto DateTime, que é então formatado para o formato 'Y-m-d' e exibido.

## Deep Dive

A necessidade de analisar datas surge principalmente do fato de que os dados são frequentemente armazenados como strings. Na versão anterior do PHP, tínhamos que usar `strtotime()` e `date()`. Mas essas funções eram pouco confiáveis para formatos de data complexos.

Em contrapartida, a função `date_create_from_format()` do PHP fornece um meio muito mais confiável de parsear datas, permitindo que você especifique a formatação de entrada.

Alternativamente, podemos usar a função `DateTime::createFromFormat()`, que é um método do objeto DateTime:

```PHP
$stringDeData = "21-12-2022";
$objetoDeData = DateTime::createFromFormat('d-m-Y', $stringDeData);
echo $objetoDeData->format('Y-m-d');

// Output: 2022-12-21
```

## Veja Também

- Documentação sobre a função `date_create_from_format()`: https://www.php.net/manual/pt_BR/function.date-create-from-format.php
- Documentação sobre a função `DateTime::createFromFormat()`: https://www.php.net/manual/pt_BR/datetime.createfromformat.php