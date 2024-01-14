---
title:    "PHP: Convertendo uma data em uma string"
keywords: ["PHP"]
---

{{< edit_this_page >}}

## Por que converter uma data em uma string?

Ao trabalhar com desenvolvimento web, muitas vezes é necessário manipular datas. No entanto, o formato padrão de data em bancos de dados ou em outras fontes pode não ser o mais adequado para exibição ao usuário. Por isso, é necessário conhecer como converter uma data em uma string, a fim de apresentá-la de maneira mais amigável e personalizada.

## Como fazer

Para converter uma data em uma string em PHP, é necessário utilizar a função `date_format()` e informar os parâmetros corretos. Por exemplo:

```PHP
<?php
$timestamp = strtotime('2021-05-07'); // converte a data para o formato timestamp
echo date_format($timestamp, 'd/m/Y'); // imprime a data no formato dia/mês/ano (07/05/2021)
```

É importante mencionar que o formato dos parâmetros utilizados na função `date_format()` segue a sintaxe da função `date()` do PHP, que pode variar de acordo com o servidor. Por isso, é importante consultar a documentação oficial para mais detalhes.

## Um mergulho mais profundo

Ao converter uma data em uma string, é possível personalizar ainda mais o formato da data, utilizando caracteres para representar diferentes partes, como o dia, mês ou ano. Por exemplo, o caractere `d` representa o dia com dois dígitos, o caractere `M` representa o nome abreviado do mês em inglês (Jan, Feb, Mar, etc.), e o caractere `Y` representa o ano com quatro dígitos.

Além disso, a função `date_format()` também pode ser utilizada para exibir outros idiomas, desde que sejam configurados os parâmetros corretos. Por exemplo:

```PHP
<?php
setlocale(LC_ALL, 'pt_BR.utf8'); // define o idioma como português do Brasil
echo date_format($timestamp, 'd \d\e F, Y'); // imprime a data no formato dia de mês escrito, ano (07 de maio de 2021)
```

## Veja também

- [Documentação oficial do PHP sobre a função `date_format()`](https://www.php.net/manual/pt_BR/function.date-format.php)
- [Lista de caracteres para formatação de datas em PHP](https://www.php.net/manual/pt_BR/function.date.php)