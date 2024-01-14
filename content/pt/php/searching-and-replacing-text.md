---
title:    "PHP: Buscando e substituindo texto"
keywords: ["PHP"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pt/php/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Por que buscar e substituir texto é importante para programadores PHP?

Você está trabalhando em um projeto PHP e de repente percebe que precisa mudar algumas informações que aparecem em várias partes do seu código. Em vez de procurar manualmente e alterar cada ocorrência, você pode simplesmente usar as funções de busca e substituição de texto em PHP para tornar esse processo mais eficiente e econômico. Em outras palavras, o recurso de busca e substituição de texto é uma ferramenta poderosa que ajuda a economizar tempo e esforço em projetos de programação em PHP.

## Como usar as funções de busca e substituição de texto em PHP

Para buscar e substituir texto em PHP, você pode usar as funções `str_replace()` ou `preg_replace()`. Ambas as funções aceitam três parâmetros: a string de busca, a string de substituição e a string na qual a busca e substituição serão realizadas. Veja um exemplo de código:

```PHP
<?php
$texto = "Hoje é um lindo dia para programar em PHP.";
$texto_novo = str_replace("lindo", "maravilhoso", $texto);
echo $texto_novo;
?>

// Output: Hoje é um maravilhoso dia para programar em PHP.
```

Você também pode usar expressões regulares no segundo parâmetro das funções `str_replace()` e `preg_replace()`, o que permite fazer substituições mais complexas e avançadas. Veja um exemplo de código usando a função `preg_replace()`:

```PHP
<?php
$texto = "O número 2021 é um ano de renovação.";
$texto_novo = preg_replace("/2021/", "2022", $texto);
echo $texto_novo;
?>

// Output: O número 2022 é um ano de renovação.
```

## Aprofundando no uso das funções de busca e substituição de texto em PHP

Além dos exemplos básicos de uso das funções de busca e substituição de texto em PHP, também é importante entender como funcionam as expressões regulares. Elas permitem realizar buscas mais precisas e substituições complexas em textos. Por exemplo, você pode usar quantificadores, próprios das expressões regulares, para especificar quantas vezes uma determinada palavra ou caractere deve ser substituído.

Outra dica importante é o uso do parâmetro opcional `$count` nas funções `str_replace()` e `preg_replace()`. Ele conta o número de substituições realizadas na string e pode ser usado para acompanhar o progresso do processo de busca e substituição.

## Veja também

- [Documentação oficial do PHP sobre a função str_replace()](https://www.php.net/manual/pt_BR/function.str-replace.php)
- [Documentação oficial do PHP sobre a função preg_replace()](https://www.php.net/manual/pt_BR/function.preg-replace.php)
- [Tutorial sobre expressões regulares em PHP](https://www.devmedia.com.br/expressoes-regulares-em-php/24012)