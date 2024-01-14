---
title:                "PHP: Extraindo subtrings"
programming_language: "PHP"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/php/extracting-substrings.md"
---

{{< edit_this_page >}}

## Por que fazer extração de subcadeias?

Fazer extração de subcadeias pode ser útil em diversas situações de programação, como na manipulação de dados de entrada ou na formatação de strings. Com a extração de subcadeias, você pode selecionar apenas a parte necessária de uma string para ser usada em seu código.

## Como fazer extração de subcadeias em PHP

Fazer a extração de subcadeias em PHP é simples e pode ser feita usando a função `substr()`. Essa função requer dois parâmetros: a string de onde a subcadeia será extraída e a posição inicial da subcadeia. Por exemplo:

```PHP
$string = "Esta é uma string para a demonstração de extração de subcadeias.";
$subcadeia = substr($string, 20);
```

A variável `$subcadeia` agora contém a subcadeia a partir da posição 20 da string original. Você também pode especificar um terceiro parâmetro opcional para determinar o tamanho da subcadeia a ser extraída. Por exemplo:

```PHP
$subcadeia = substr($string, 20, 10);
```

Nesse caso, a subcadeia terá 10 caracteres a partir da posição 20 da string original. O resultado da variável `$subcadeia` será "para a demo".

## Mais detalhes sobre a extração de subcadeias

Além da função `substr()`, existem outras formas de fazer a extração de subcadeias em PHP. Por exemplo, você também pode usar a função `mb_substr()` para trabalhar com strings multibyte e tratar caracteres acentuados corretamente.

Outra opção é utilizar expressões regulares para fazer a extração de subcadeias. Isso pode ser especialmente útil quando você precisa buscar por padrões específicos na string original.

Em qualquer um dos casos, é importante lembrar que a posição da subcadeia é sempre contada a partir do índice 0, ou seja, o primeiro caractere da string é a posição 0, o segundo é a posição 1 e assim por diante.

## Veja também

- [Documentação oficial do PHP sobre a função `substr()`](https://www.php.net/manual/pt_BR/function.substr.php)
- [Documentação oficial do PHP sobre a função `mb_substr()`](https://www.php.net/manual/pt_BR/function.mb-substr.php)
- [Guia sobre expressões regulares em PHP](https://www.php.net/manual/pt_BR/book.pcre.php)