---
title:                "Analisando uma data a partir de uma string"
date:                  2024-02-03T19:14:51.193404-07:00
model:                 gpt-4-0125-preview
simple_title:         "Analisando uma data a partir de uma string"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/php/parsing-a-date-from-a-string.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## O que & Por quê?

Analisar uma data a partir de uma string em PHP envolve converter um texto que representa uma data e/ou hora em um objeto `DateTime` do PHP ou outros formatos de data/hora. Isso é crucial para fins de validação, manipulação, armazenamento e apresentação de dados, especialmente ao trabalhar com entradas de usuários ou dados de fontes externas.

## Como fazer:

A classe integrada `DateTime` do PHP fornece um conjunto poderoso de funções para analisar e trabalhar com datas. Você pode criar uma instância de `DateTime` a partir de uma string de data usando o construtor, e então formatá-la conforme necessário. Veja como:

```php
$dateString = "2023-04-25 15:30:00";
$dateObject = new DateTime($dateString);

echo $dateObject->format('Y-m-d H:i:s');
// Saída: 2023-04-25 15:30:00
```

Para lidar com strings que seguem formatos não padrão, você pode usar o método `createFromFormat`, que permite especificar o formato exato da data de entrada:

```php
$dateString = "25-04-2023 3:30 PM";
$dateObject = DateTime::createFromFormat('d-m-Y g:i A', $dateString);

echo $dateObject->format('Y-m-d H:i:s');
// Saída: 2023-04-25 15:30:00
```

Para análises mais complexas que podem não ser diretamente suportadas por `DateTime`, o PHP oferece a função `strtotime`, que tenta analisar qualquer descrição textual inglesa de data/hora em um timestamp Unix:

```php
$timestamp = strtotime("next Thursday");
echo date('Y-m-d', $timestamp);
// A saída variará dependendo da data atual, por exemplo, "2023-05-04"
```

**Usando bibliotecas de terceiros:**

Embora as funções integradas do PHP cubram uma ampla gama de casos de uso, às vezes você pode precisar de capacidades de análise mais sofisticadas. A biblioteca Carbon, uma extensão da classe DateTime do PHP, oferece um rico conjunto de recursos para manipulação de data/hora:

```php
require 'vendor/autoload.php';

use Carbon\Carbon;

$dateString = "Tomorrow";
$date = Carbon::parse($dateString);

echo $date->toDateTimeString();
// A saída variará, por exemplo, "2023-04-26 00:00:00"
```

O método `parse` do Carbon pode lidar de forma inteligente com uma infinidade de formatos de data e hora, tornando-o uma ferramenta inestimável para aplicações que exigem uma funcionalidade de análise de data flexível.
