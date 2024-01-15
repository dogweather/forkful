---
title:                "Convertendo uma string em maiúsculas"
html_title:           "PHP: Convertendo uma string em maiúsculas"
simple_title:         "Convertendo uma string em maiúsculas"
programming_language: "PHP"
category:             "PHP"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/php/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Por que

Você já se deparou com um texto em que todas as palavras estão com as letras maiúsculas? Isso pode ser confuso e difícil de ler. Em alguns casos, é necessário capitalizar apenas a primeira letra de cada palavra em um texto, seja por questões estéticas ou de formatação. Neste artigo, vamos aprender a como capitalizar uma string em PHP de forma simples e eficiente.

## Como Fazer

Para capitalizar uma string em PHP, podemos utilizar a função `ucwords()`. Esta função recebe como parâmetro a string que desejamos capitalizar e retorna uma nova string com a primeira letra de cada palavra em maiúscula. Veja um exemplo abaixo:

```PHP
$string = "ola mundo";
echo ucwords($string);
```

**Saída: Ola Mundo**

Além disso, também é possível utilizar a função `ucfirst()`, que capitaliza apenas a primeira letra de uma string. Veja um exemplo:

```PHP
$string = "ola mundo";
echo ucfirst($string);
```

**Saída: Ola mundo**

A função `ucwords()` é útil para casos em que precisamos capitalizar mais de uma palavra e `ucfirst()` para casos em que é necessário capitalizar apenas a primeira letra de uma sentença.

Uma outra opção é utilizar a função `mb_convert_case()`, que permite escolher entre capitalizar todas as letras de uma string, apenas a primeira letra de cada palavra ou apenas a primeira letra de uma sentença. Veja um exemplo:

```PHP
$string = "ola mundo";
echo mb_convert_case($string, MB_CASE_TITLE, "UTF-8");
```

**Saída: Ola Mundo**

## Deep Dive

Para entendermos melhor como essas funções funcionam, é importante saber que o PHP possui uma função interna chamada `ord()` que retorna o valor ASCII de um caractere. O valor ASCII para as letras minúsculas varia de 97 a 122 e para as letras maiúsculas varia de 65 a 90.

Ao utilizar a função `ucwords()` ou `ucfirst()`, o PHP verifica o valor ASCII de cada caractere na string e, caso o valor esteja entre 97 e 122, é feita uma subtração de 32 para transformar o caractere em sua letra maiúscula correspondente.

Já a função `mb_convert_case()` utiliza uma biblioteca de caracteres interna do PHP e não a tabela ASCII, o que permite que ela consiga lidar com caracteres multibyte, como os acentuados.

## Veja Também

- Documentação oficial do PHP sobre a função `ucwords()`: http://php.net/manual/pt_BR/function.ucwords.php
- Documentação oficial do PHP sobre a função `ucfirst()`: http://php.net/manual/pt_BR/function.ucfirst.php
- Documentação oficial do PHP sobre a função `mb_convert_case()`: http://php.net/manual/pt_BR/function.mb-convert-case.php