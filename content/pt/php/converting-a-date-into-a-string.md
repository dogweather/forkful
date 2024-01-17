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

## O que é e por que fazer?
Converter uma data em uma string simplesmente significa transformar uma data em um formato mais legível para humanos. Programadores geralmente fazem isso para facilitar a visualização e manipulação de datas em aplicações web e de desktop.

## Como fazer:
Para converter uma data em uma string no PHP, existem duas opções: utilizar a função nativa `date()`, que aceita um formato de data como parâmetro, ou usar a classe `DateTime` seguida do método `format()`, que também aceita um formato de data como parâmetro.

```
// Utilizando a função date()
$data = date('d/m/Y'); // 14/04/2020

// Utilizando a classe DateTime
$data = new DateTime();
echo $data->format('d/m/Y'); // 14/04/2020
```

## Mergulho Profundo:
Antes do PHP 5.1, a única maneira de converter uma data em uma string era utilizando a função `strftime()`. Porém, esta função era limitada a sistemas operacionais específicos e sujeita a erros de localização. Com a introdução da função `date()`, esses problemas foram resolvidos, tornando-a a forma recomendada para converter datas em strings no PHP.

Além disso, também é possível utilizar a classe `DateTime` para trabalhar com datas de forma mais precisa e flexível, permitindo, por exemplo, a manipulação e cálculo de intervalos de tempo.

## Veja também:
- Documentação do PHP sobre a função `date()`: https://www.php.net/manual/pt_BR/function.date.php
- Documentação do PHP sobre a classe `DateTime`: https://www.php.net/manual/pt_BR/class.datetime
- Documentação do PHP sobre a função `strftime()`: https://www.php.net/manual/pt_BR/function.strftime