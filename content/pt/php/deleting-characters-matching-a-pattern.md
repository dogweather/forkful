---
title:                "Exclusão de caracteres correspondentes a um padrão"
html_title:           "PHP: Exclusão de caracteres correspondentes a um padrão"
simple_title:         "Exclusão de caracteres correspondentes a um padrão"
programming_language: "PHP"
category:             "PHP"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/php/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## O que & Por que?
Às vezes, um programador pode precisar excluir certos caracteres ou padrões de uma string em PHP. Isso pode ser útil para limpar dados ou formatar informações de maneira específica. É uma técnica comum usada para manipulação de string.

## Como fazer:
Um exemplo simples de como excluir caracteres correspondentes a um padrão em uma string é usando a função `preg_replace()` em PHP. Abaixo está um exemplo de código que substitui todos os números de um número de telefone por asteriscos:

```PHP
$telefone = "123-456-7890";
$telefone = preg_replace("/[0-9]/", "*", $telefone);
echo $telefone; // output: ***-***-****
```

## Mergulho Profundo:
Deletar caracteres correspondentes a um padrão é uma técnica que sempre foi usada na programação, mas se tornou mais fácil com o uso de funções específicas, como `preg_replace()` em PHP. Existem outras opções, como usar `str_replace()` ou `substr()`, mas nem sempre são tão precisas ou eficientes quanto a função `preg_replace()`.

Na implementação, a função `preg_replace()` usa expressões regulares para especificar o padrão a ser correspondido e a string a ser usada como substituição. Isso permite uma ampla gama de possibilidades na manipulação de string, tornando-a uma ferramenta poderosa para os programadores.

## Veja também:
- Documentação oficial do PHP sobre `preg_replace()`: https://www.php.net/manual/en/function.preg-replace.php
- Uma introdução às expressões regulares em PHP: https://www.php.net/manual/en/regexp.introduction.php
- Uma comparação entre `preg_replace()` e outras funções de manipulação de string: https://stackoverflow.com/questions/4178155/what-is-the-difference-between-preg-replace-str-replace-and-strtr