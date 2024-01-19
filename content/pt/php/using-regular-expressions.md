---
title:                "Usando expressões regulares"
html_title:           "Gleam: Usando expressões regulares"
simple_title:         "Usando expressões regulares"
programming_language: "PHP"
category:             "PHP"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/php/using-regular-expressions.md"
---

{{< edit_this_page >}}

## O Que & Por quê?

Expressões regulares, ou regex, são sequências de caracteres que formam um padrão de busca. Programadores as usam para economizar tempo, implementando buscas e substituições de texto de uma forma rápida e fácil.

## Como Fazer:

Vamos usar a função `preg_match()` do PHP para um exemplo simples de expressão regular. Observe que a expressão regular é colocada entre barras.

```PHP
<?php
  $str = "Olá, Programador!";
  $padrao = "/Programador/"; 
  if (preg_match($padrao, $str)) {
    echo "A palavra foi encontrada!";
  } else {
    echo "A palavra não foi encontrada!";
  }
?>
```

Neste exemplo, estamos procurando a palavra "Programador" na string. A saída será:

```PHP
A palavra foi encontrada!
```

## Mergulho Profundo:

Expressões regulares foram introduzidas pela primeira vez na década de 1950, mas só tiveram seu potencial plenamente reconhecido na década de 1960. 

As alternativas ao uso de expressões regulares incluem a busca literal de strings e o uso de funções de manipulação de strings, mas nenhuma delas oferece a mesma flexibilidade e eficiência que as expressões regulares.

Muitos programas PHP internos usam bibliotecas de expressões regulares, como PCRE (Perl Compatible Regular Expressions), para implementar sua funcionalidade. Elas tornam as expressões regulares uma ferramenta valiosa para qualquer programador PHP.

## Veja Também:

Para mais informações sobre expressões regulares em PHP, confira os seguintes links:

- [PHP Manual - Preg Functions](https://www.php.net/manual/en/ref.pcre.php)
- [Regular-Expressions.info - Tutorial](https://www.regular-expressions.info/tutorial.html)
- [Regex101 - Online Regex Tester](https://regex101.com/)