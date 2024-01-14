---
title:                "PHP: Capitalizando uma string"
simple_title:         "Capitalizando uma string"
programming_language: "PHP"
category:             "PHP"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/php/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Por que capitalizar uma string em PHP?

Capitalizar uma string em PHP é uma tarefa comum em muitos projetos de desenvolvimento web. Isso permite que o texto seja formatado corretamente, tornando-o mais legível e profissional. Além disso, pode ser necessário capitalizar nomes de usuários ou títulos de artigos em um sistema de gerenciamento de conteúdo. Neste artigo, vamos explorar como capitalizar uma string em PHP e o porquê dessa prática ser importante.

## Como capitalizar uma string em PHP

A função `ucwords()` do PHP pode ser usada para capitalizar uma string. Esta função recebe uma string como parâmetro e retorna uma nova string com a primeira letra de cada palavra em maiúsculo. Veja um exemplo:

```PHP
<?php
$titulo = "como capitalizar uma string em PHP";
echo ucwords($titulo); // Saída: Como Capitalizar Uma String Em PHP
```

Outra opção é usar a função `mb_convert_case()` que permite especificar como a capitalização deve ser feita (maiúscula, minúscula ou título). Essa função é especialmente útil quando se trabalha com caracteres multibyte, como no caso do idioma português. Veja um exemplo:

```PHP
<?php
$artigo = "aprendendo a programar em PHP";
echo mb_convert_case($artigo, MB_CASE_TITLE, "UTF-8"); // Saída: Aprendendo a Programar em PHP
```

## Mergulho Profundo

A função `ucwords()` é uma das funções mais fáceis de usar para capitalizar uma string em PHP. Ela funciona corretamente com a maioria dos idiomas e caracteres multibyte. No entanto, é importante lembrar que essa função não é indicada para casos em que é necessário ter controle mais preciso sobre a capitalização, como em nomes próprios ou abreviações. Nestes casos, a função `mb_convert_case()` pode ser mais adequada.

Também é importante mencionar que a capitalização de uma string em PHP é sensível ao idioma configurado no servidor. Por exemplo, se o servidor estiver configurado para o idioma inglês, a função `ucwords()` irá capitalizar apenas a primeira palavra de uma string, enquanto se estiver configurado para o português, ela irá capitalizar todas as primeiras letras de cada palavra.

## Veja também

- [Função `ucwords()` no site oficial do PHP](https://www.php.net/manual/pt_BR/function.ucwords.php)
- [Função `mb_convert_case()` no site oficial do PHP](https://www.php.net/manual/pt_BR/function.mb-convert-case.php)
- [Acentuação nas strings em PHP](https://www.alura.com.br/artigos/acentuacao-nas-strings-em-php)