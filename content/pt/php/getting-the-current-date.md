---
title:                "Obtendo a data atual"
html_title:           "PHP: Obtendo a data atual"
simple_title:         "Obtendo a data atual"
programming_language: "PHP"
category:             "PHP"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/php/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Por que

Você já se perguntou como muitos sites e aplicativos mostram a data atual? A resposta simples é que eles usam recursos de programação para obter a data atual e exibi-la em um formato legível para os usuários. Neste artigo, vamos falar sobre como você pode fazer isso usando o PHP, a linguagem de script do lado do servidor mais popular e versátil.

## Como fazer

Para obter a data atual em PHP, você pode usar a função nativa `date()`. Esta função aceita dois parâmetros: um formato de data e um carimbo de data e hora opcional. Aqui está um exemplo de como usar a função `date()` para mostrar a data atual em formato simples:

```PHP
<?php
echo "Hoje é: " . date("d/m/Y");
?>
// Output: Hoje é: 30/06/2020
```

Você também pode personalizar o formato de data de acordo com suas preferências. Por exemplo, se você quiser mostrar a data por extenso, pode usar o seguinte formato:

```PHP
<?php
echo "Hoje é: " . date("d \\d\\e F \\d\\e Y");
?>
// Output: Hoje é: 30 de junho de 2020
```

Você pode ver uma lista completa de formatos de data possíveis na documentação oficial do PHP.

## Deep Dive

Você pode estar se perguntando como o PHP obtém a data atual. Na verdade, o PHP se baseia no relógio do servidor para obter a data e hora atual. Isso significa que, se a data e hora do servidor estiverem incorretas, sua função `date()` também estará incorreta.

Além disso, você também pode usar outras funções de data e hora, como `time()`, para obter o carimbo de data e hora atual em segundos. Isso é útil quando você precisa fazer cálculos ou comparar datas.

Além disso, é importante notar que a função `date()` sempre retorna a data e hora atual com base no fuso horário do servidor. Portanto, se você estiver em um fuso horário diferente, precisará configurá-lo manualmente usando a função `date_default_timezone_set()`.

## Veja também

- [Documentação oficial do PHP sobre a função `date()`](https://www.php.net/manual/pt_BR/function.date.php)
- [Lista completa de formatos de data em PHP](https://www.php.net/manual/pt_BR/datetime.format.php)
- [Função `time()` em PHP](https://www.php.net/manual/pt_BR/function.time.php)
- [Como alterar o fuso horário em PHP](https://www.php.net/manual/pt_BR/function.date-default-timezone-set.php)

Esperamos que este artigo tenha sido útil para você entender como obter a data atual em PHP e como personalizar seu formato. Se você quiser saber mais sobre programação em PHP, recomendamos consultar a documentação oficial e experimentar diferentes funções para melhorar suas habilidades de programação. Até a próxima!