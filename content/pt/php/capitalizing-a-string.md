---
title:                "Maiúscula de uma string"
html_title:           "PHP: Maiúscula de uma string"
simple_title:         "Maiúscula de uma string"
programming_language: "PHP"
category:             "PHP"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/php/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## O que é e por quê?

Capitalizar uma string é transformar a primeira letra de cada palavra em maiúscula. Isso pode ser útil para deixar a string legível e padronizada, especialmente em nomes e títulos. Programadores muitas vezes fazem isso para melhorar a organização e legibilidade do código.

## Como fazer:

```
<?php
$string = "esse é um exemplo de string";
echo ucfirst($string); // Saída: "Esse é um exemplo de string"
echo ucwords($string); // Saída: "Esse É Um Exemplo De String"
?>
```

## Mergulho profundo:

Capitalizar strings é uma prática comum na programação e tem suas raízes na linguagem de programação C. A função ucfirst () foi introduzida no PHP para capitalizar a primeira letra de uma string, enquanto ucwords () foi criada para capitalizar todas as palavras de uma string. Outras alternativas incluem strtoupper () para transformar toda a string em maiúsculas e strtolower () para convertê-la em minúsculas. Essas funções são muito úteis ao trabalhar com entradas de usuário, por exemplo.

## Veja também:

- Documentação oficial do PHP para funções de manipulação de strings: https://www.php.net/manual/pt_BR/ref.strings.php
- Tabela de caracteres ASCII: https://www.w3schools.com/charsets/ref_utf_ascii.asp