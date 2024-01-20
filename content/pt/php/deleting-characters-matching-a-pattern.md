---
title:                "Excluindo caracteres que correspondem a um padrão"
html_title:           "Arduino: Excluindo caracteres que correspondem a um padrão"
simple_title:         "Excluindo caracteres que correspondem a um padrão"
programming_language: "PHP"
category:             "PHP"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/php/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

# Removendo caracteres correspondentes a um padrão em PHP

## O que & Por quê?

Remover caracteres que correspondem a um padrão é uma forma de otimizar dados em texto, tornando-os mais úteis e fáceis para manipulação. Programadores fazem isso para limpar ou filtrar inputs, corrigir erros de digitação ou uniformizar formatos de dados.

## Como fazer:

Em PHP, usamos a função `preg_replace()` para isso. O formato geral é `preg_replace(pattern, replacement, string)`. Se o padrão for localizado na string, será substituído.

```PHP
$string = "Hello, Programador!";
$pattern = "/o/";
$replacement = "";
echo preg_replace($pattern, $replacement, $string);
```

A saída seria:

```PHP
Hell, Prgramadr!
```

Na string "Hello, Programador!", todos os 'o' foram removidos.

## Mergulhando fundo:

A função `preg_replace()` foi introduzida no PHP 4 e tem sido fundamental para manipulação de strings desde então. É mais poderosa do que a função `str_replace()` porque permite o uso de expressões regulares.

Outras alternativas são `preg_replace_callback()`, que permite usar uma função de callback para substituições, e `preg_filter()`, que retorna somente as strings modificadas.

Note que `preg_replace()` pode gerar problemas de desempenho com strings muito grandes devido às retroreferências, especialmente quando usamos grupos de captura na expressão regular.

## Veja também:

A documentação PHP é um ótimo recurso para informações adicionais e exemplos de código:

- [preg_replace na documentação PHP](https://www.php.net/manual/pt_BR/function.preg-replace)
- [preg_replace_callback na documentação PHP](https://www.php.net/manual/pt_BR/function.preg-replace-callback)
- [preg_filter na documentação PHP](https://www.php.net/manual/pt_BR/function.preg-filter)

Outros recursos úteis para aprender mais sobre expressões regulares em PHP:

- [Tutorial RegEx no W3Schools](https://www.w3schools.com/php/php_regex.asp)
- [Guia de Expressões Regulares da PHP.net](https://www.php.net/manual/pt_BR/book.pcre.php)