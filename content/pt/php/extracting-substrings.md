---
title:                "Extraindo substrings"
html_title:           "Bash: Extraindo substrings"
simple_title:         "Extraindo substrings"
programming_language: "PHP"
category:             "PHP"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/php/extracting-substrings.md"
---

{{< edit_this_page >}}

## O Que e Por Quê?

Extrair substrings é o processo de pegar uma parte específica de uma string. Programadores fazem isso para manipular, analisar ou transformar os dados de maneira mais eficaz.

## Como Fazer:

A função `substr` é frequentemente usada para extrair uma substring.

```PHP
<?php
$string = 'Olá, mundo!';
$substring = substr($string, 4, 5);
echo $substring;  // retorno: ', mun'
?>
```

No exemplo acima, `substr` pega a string 'Olá, mundo!' e extrai a substring começando na posição 4 (0 based) e com comprimento 5.

## Mergulho Profundo

**Contexto histórico**: Em versões anteriores do PHP, `strstr` foi usada para extrair substrings, mas era menos eficiente que `substr`.

**Alternativas**: Além do `substr`, a função `mb_substr` pode ser usada para suportar o multibyte, o que é importante ao lidar com caracteres não latinos.

```PHP
<?php
$string = 'Olá, mundo!';
$substring = mb_substr($string, 4, 5, "UTF-8");
echo $substring;
?>
```

**Detalhes de implementação**: Em PHP, as strings são zero-indexadas, o que significa que o primeiro caractere é na posição 0. 

## Veja Também

Informações adicionais podem ser encontradas na documentação oficial do PHP para: 
- [substr](https://www.php.net/manual/pt_BR/function.substr.php)
- [mb_substr](https://www.php.net/manual/pt_BR/function.mb-substr.php)

Lembrando que, em programação, sempre existem várias maneiras de resolver um problema. A extração de substrings é apenas uma maneira de manipular strings. Considere outras funções PHP como `strpos`, `strstr` e `str_split` para diferentes necessidades de manipulação.