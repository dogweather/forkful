---
title:                "Capitalizando uma string"
html_title:           "PHP: Capitalizando uma string"
simple_title:         "Capitalizando uma string"
programming_language: "PHP"
category:             "PHP"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/php/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## O que é & Por que?

Capitalizar uma string significa transformar as primeiras letras de cada palavra em maiúsculas. Programadores fazem isso principalmente para melhorar a estética e a legibilidade de textos em aplicativos e websites.

## Como Fazer:

No PHP, usamos a função `ucwords()` para capitalizar cada palavra de uma string. Veja abaixo:

```PHP
<?php
    $frase = "aprendendo php é divertido";
    echo ucwords($frase);
?>
```

Resultado da execução será: "Aprendendo Php É Divertido".

## Mergulho Profundo:

A função `ucwords()` do PHP foi introduzida pela primeira vez no PHP 4 e carrega consigo um longo histórico de uso pelos programadores PHP em todo o mundo. 
Uma alternativa a essa função, é o `mb_convert_case()` que também capitaliza uma string, mas respeita caracteres multibyte, útil para idiomas com casos de caracteres especiais. Por exemplo:

```PHP
<?php
    $frase = "olá, mundo!";
    echo mb_convert_case($frase, MB_CASE_TITLE, "UTF-8");
?>
```

Isso produzirá: "Olá, Mundo!". 

A implementação interna da função `ucwords()` é bastante direta, ela itera sobre a string de entrada e capitaliza cada caractere que segue um espaço em branco.

## Veja também:

1. PHP Manual - ucwords() : https://www.php.net/manual/en/function.ucwords.php
2. PHP Manual - mb_convert_case() : https://www.php.net/manual/en/function.mb-convert-case.php
3. Stack Overflow - Capitalize the first letter of both words in a two word string: https://stackoverflow.com/questions/2828726/capitalize-the-first-letter-of-both-words-in-a-two-word-string