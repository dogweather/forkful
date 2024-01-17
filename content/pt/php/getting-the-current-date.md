---
title:                "Obtendo a data atual."
html_title:           "PHP: Obtendo a data atual."
simple_title:         "Obtendo a data atual."
programming_language: "PHP"
category:             "PHP"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/php/getting-the-current-date.md"
---

{{< edit_this_page >}}

## O que é e por que os programadores utilizam data atual em PHP?

A obtenção da data atual em PHP é uma funcionalidade importante para programadores que trabalham com datas e horários. Isso permite que eles possam gerenciar e manipular informações temporais de forma eficiente em seus projetos.

## Como fazer:

```
<?php
  // Utilizando a função date() para obter a data atual
  echo "Hoje é " . date("d/m/Y") . "<br>";

  // Saída: Hoje é 23/08/2021

  // Obtendo a data atual com formatação customizada
  echo "Hoje é " . date("l jS \of F Y h:i:s A") . "<br>";

  // Saída: Hoje é segunda-feira 23º de agosto 2021 03:11:14 PM
?>
```

## Aprofundando:

A obtenção da data atual em PHP é possível graças à função date(), que pode ser combinada com formatos de tempo para personalizar a saída de acordo com as necessidades do programador. Além disso, existem outras funções alternativas em PHP para obter informações temporais, como time() e strtotime().

## Veja também:

- Referência da função date() em PHP: https://www.php.net/manual/pt_BR/function.date.php
- Alternativas para obtenção da data atual em PHP: https://www.php.net/manual/pt_BR/function.time.php e https://www.php.net/manual/pt_BR/function.strtotime.php