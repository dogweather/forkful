---
title:                "Extraindo Substrings"
html_title:           "PHP: Extraindo Substrings"
simple_title:         "Extraindo Substrings"
programming_language: "PHP"
category:             "PHP"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/php/extracting-substrings.md"
---

{{< edit_this_page >}}

## O que & Por quê?
Extrair substrings é um processo comum em programação que envolve a obtenção de um fragmento de uma string maior. Muitas vezes, os programadores fazem isso para manipular dados ou para pesquisar informações específicas dentro de uma string.

## Como fazer:
```PHP
// Exemplo 1: Extrair os primeiros 5 caracteres de uma string
$string = "Olá mundo";
$sub = substr($string, 0, 5);
echo $sub; // Saída: "Olá m"

// Exemplo 2: Extrair os últimos 3 caracteres de uma string
$string = "Lorem ipsum dolor";
$sub = substr($string, -3);
echo $sub; // Saída: "or"

// Exemplo 3: Extrair uma substring de uma posição inicial até o fim da string
$string = "Lorem ipsum dolor";
$sub = substr($string, 6);
echo $sub; // Saída: "ipsum dolor"
```

## Mergulho Profundo:
Extrair substrings é uma técnica amplamente utilizada em programação moderna. Ela tem suas raízes em linguagens de programação antigas, como o C, e foi adaptada para outras linguagens, como o PHP. Embora seja uma abordagem comum, existem algumas opções alternativas, como a função `str_split()` do PHP ou o uso de expressões regulares. Ao extrair substrings, é importante ter em mente que a posição de início e o comprimento da string são contados a partir de 0, e que valores negativos são permitidos para começar a contagem a partir do final da string.

## Veja também:
- [Documentação do PHP sobre a função `substr()`](https://www.php.net/manual/pt_BR/function.substr.php)
- [Artigo sobre a função `str_split()` em PHP](https://www.php.net/manual/pt_BR/function.str-split.php)
- [Guia básico sobre expressões regulares em PHP](https://www.php.net/manual/pt_BR/reference.pcre.pattern.syntax.php)