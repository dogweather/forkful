---
title:                "PHP: Convertendo uma string para letras minúsculas"
programming_language: "PHP"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/php/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Por que converter uma string para letras minúsculas?

Converter uma string para letras minúsculas é uma tarefa comum em programação, especialmente quando se trabalha com entradas de usuário. Isso garante que não haja problemas de comparação de strings ou erros ao digitar algum texto com letras maiúsculas acidentalmente.

## Como fazer isso em PHP?

Para converter uma string em letras minúsculas em PHP, podemos utilizar a função `strtolower()`. Veja um exemplo abaixo:

```PHP
$string = "Olá MUNDO!";
echo strtolower($string);
```

O código acima irá imprimir "olá mundo!", já que a função `strtolower()` transforma todas as letras maiúsculas em minúsculas.

## Aprofundando mais

É importante lembrar que a conversão para letras minúsculas pode variar de acordo com a codificação de caracteres utilizada. Por exemplo, em UTF-8, o caractere "É" será convertido para "é", mas em ISO-8859-1, ele será convertido para "ê".

Também é possível converter apenas a primeira letra de uma string para minúscula, utilizando a função `lcfirst()`. E, caso seja necessário converter todas as primeiras letras das palavras em uma string para minúsculas, podemos utilizar a função `ucwords()`.

## Veja também

- Documentação oficial do PHP para a função `strtolower()`: https://www.php.net/manual/pt_BR/function.strtolower.php
- Mais informações sobre conversão de strings no PHP: https://www.php.net/manual/pt_BR/reference.mbstring.php