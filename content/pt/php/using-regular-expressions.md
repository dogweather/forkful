---
title:                "Utilizando expressões regulares"
html_title:           "Bash: Utilizando expressões regulares"
simple_title:         "Utilizando expressões regulares"
programming_language: "PHP"
category:             "PHP"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/php/using-regular-expressions.md"
---

{{< edit_this_page >}}

## What & Why? (O Que & Por Quê?)
Regular expressions, ou regex, são uma ferramenta para busca e manipulação de texto usando padrões definidos. Programadores usam regex para validação de dados, busca, substituição e parsing de textos de forma eficiente e concisa.

## How to: (Como Fazer:)
```PHP
<?php
$texto = 'O email do contato é contato@exemplo.com';
$padrao = '/[a-z0-9._%+-]+@[a-z0-9.-]+\.[a-z]{2,}/';

if (preg_match($padrao, $texto, $matches)) {
    echo "Email encontrado: " . $matches[0];
} else {
    echo "Nenhum email válido encontrado.";
}
?>
```
Saída: Email encontrado: contato@exemplo.com

## Deep Dive (Mergulho Profundo)
As expressões regulares originaram-se na década de 1950. Alternativas incluem métodos de string como strpos() e strstr() no PHP, mas eles não oferecem a mesma flexibilidade. Quanto à implementação, o PHP usa a biblioteca PCRE (Perl Compatible Regular Expressions) para processar regex, o que proporciona uma rica variedade de opções e sintaxe.

## See Also (Veja Também)
- Documentação oficial do PHP sobre regex: [PHP PCRE](https://www.php.net/manual/pt_BR/book.pcre.php)
- Tutorial interativo de regex: [Regex101](https://regex101.com/)
- Artigo sobre expressões regulares: [Regular Expressions in PHP](https://www.phpliveregex.com/)