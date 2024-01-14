---
title:                "PHP: Obtendo a data atual"
simple_title:         "Obtendo a data atual"
programming_language: "PHP"
category:             "PHP"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/php/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Por que obter a data atual é importante na programação PHP?

Obter a data atual em um programa PHP é extremamente importante para uma variedade de tarefas, como gerenciamento de dados, rastreamento de eventos e agendamento de tarefas. Além disso, a data atual pode ser usada para criar pagamentos automáticos, notificações de prazos e muito mais. É uma função essencial na maioria dos aplicativos e sites modernos.

## Como obter a data atual em PHP?

A maneira mais simples de obter a data atual em PHP é usando a função `date()`. Esta função permite que você especifique o formato da data que deseja obter e retornará a data atual no formato especificado. Veja o exemplo abaixo:

```
<?php
$data = date('d/m/Y');
echo $data; // output: 14/07/2021
```

Você também pode usar outros caracteres específicos para formatar a data de acordo com suas necessidades. Por exemplo, `D` retorna o dia abreviado da semana (ex: Wed), `M` retorna o mês abreviado (ex: Jul) e `Y` retorna o ano com 4 dígitos (ex: 2021). Você pode encontrar uma lista completa de caracteres de formatação em [PHP Manual](https://www.php.net/manual/en/function.date.php).

## Mergulho profundo

Além da função `date()`, o PHP também possui outras funções para manipular e trabalhar com datas, como `strtotime()` e `mktime()`. A função `strtotime()` converte uma string de data em um timestamp UNIX, que pode ser usado para comparar datas ou realizar cálculos. Já a função `mktime()` retorna um timestamp UNIX com base nos valores especificados para hora, minuto, segundo, mês, dia e ano.

Também é importante lembrar que o PHP trabalha com fusos horários e configurações de data globais. Você pode especificar o fuso horário a ser usado em seu código usando a função `date_default_timezone_set()`. Além disso, o PHP permite que você altere o formato de data global usando a função `setlocale()`.

## Veja também
- [Documentação oficial do PHP sobre a função date](https://www.php.net/manual/en/function.date.php)
- [Tutorial sobre como trabalhar com datas em PHP](https://www.php.net/manual/en/datetime.settimezone.php)
- [Artigo sobre os fusos horários em PHP](https://www.php.net/manual/en/datetime.settimezone.php)