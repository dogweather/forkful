---
title:    "PHP: Traduzindo uma data em uma sequência de caracteres"
keywords: ["PHP"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pt/php/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

# Por que converter uma data em uma string?

Existem várias razões pelas quais um programador pode precisar converter uma data em uma string em PHP. Algumas dessas razões incluem:

- Exibir a data de forma legível para os usuários em um site ou aplicativo
- Armazenar a data em um banco de dados em formato de string
- Manipular a data em uma funcionalidade específica, como ordenação ou cálculos de dias

Dito isto, vamos dar uma olhada em como converter uma data em uma string em PHP.

# Como fazer

Para converter uma data em uma string em PHP, devemos primeiro usar a função `date()` e especificar o formato de saída que desejamos. Vamos dar uma olhada em alguns exemplos:

```
<?php
// Data atual em formato de string
echo "Hoje é " . date("d/m/Y") . "<br>";

// Data e hora atual em formato de string
echo "Agora são " . date("d/m/Y h:i:s A") . "<br>";

// Data e hora em uma localização específica
date_default_timezone_set("America/Sao_Paulo");
echo "A data e hora atual em São Paulo é " . date("d/m/Y h:i:s A") . "<br>";
?>
```

O código acima produz a seguinte saída:

```
Hoje é 17/03/2021
Agora são 17/03/2021 03:30:15 PM
A data e hora atual em São Paulo é 17/03/2021 03:30:15 PM
```

Você também pode personalizar o formato da string de data de acordo com suas necessidades, usando caracteres específicos para representar os diferentes elementos da data, como dia, mês, ano e hora. Aqui estão alguns exemplos:

- `d` representa o dia do mês com dois dígitos, com um zero à esquerda para números menores que 10 (por exemplo: 02, 09)
- `m` representa o mês com dois dígitos, com um zero à esquerda para meses menores que 10 (por exemplo: 07, 11)
- `Y` representa o ano com quatro dígitos
- `h` representa a hora no formato de 12 horas, com um zero à esquerda para horas menores que 10 (por exemplo: 09, 11)
- `i` representa os minutos com dois dígitos, com um zero à esquerda para minutos menores que 10 (por exemplo: 05, 59)
- `s` representa os segundos com dois dígitos, com um zero à esquerda para segundos menores que 10 (por exemplo: 03, 08)
- `A` representa o período do dia, AM ou PM

# Aprofundando mais

Além da função `date()`, existem outras funções em PHP que podem ajudar a converter uma data em uma string. Algumas delas incluem:

- `strtotime()` que converte uma string em uma data Unix. Por exemplo, `strtotime("October 13, 2015")` retorna o valor `1444713600`, que representa a data 13 de outubro de 2015 em formato Unix.
- `strftime()` que permite formatar uma data de acordo com as configurações de localidade do sistema.

É importante lembrar que, ao converter uma data em uma string, é necessário levar em consideração a configuração de localidade do sistema, pois o formato da data pode variar de acordo com a região.

# Veja também

- [Documentação oficial do PHP sobre a função date()](https://www.php.net/manual/pt_BR/function.date.php)
- [Tabelas de caracteres para formatação de datas em PHP](https://www.php.net/manual/pt_BR/function.date.php#refsect1-function.date-parameters)
- [Guia de configuração de localidade no PHP](https://www.php.net/manual/pt_BR/function.setlocale.php)