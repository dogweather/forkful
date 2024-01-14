---
title:                "PHP: Buscando e substituindo texto"
simple_title:         "Buscando e substituindo texto"
programming_language: "PHP"
category:             "PHP"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/php/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Por que utilizar a função de busca e substituição em textos?

A busca e substituição de textos é uma ferramenta muito útil para programadores PHP. Com ela, é possível realizar alterações em um texto de forma rápida e eficiente, economizando tempo e evitando erros manuais. Além disso, essa função também pode facilitar a manutenção de códigos e a automação de tarefas.

## Como utilizar a função de busca e substituição em PHP

Para utilizar a função de busca e substituição em PHP, é necessário utilizar a função `str_replace()`. Essa função possui os seguintes parâmetros: a string a ser substituída, a nova string que irá substituí-la e a string onde a busca será realizada.

Por exemplo, se quisermos substituir a palavra "amarelo" por "vermelho" em um texto, o código seria o seguinte:

```PHP
$frase = "O céu está amarelo hoje.";
$nova_frase = str_replace("amarelo", "vermelho", $frase);
echo $nova_frase;
```

O resultado seria: "O céu está vermelho hoje." Como podemos ver, a função `str_replace()` substituiu todas as ocorrências da palavra "amarelo" pela palavra "vermelho".

## Mais informações sobre a função de busca e substituição

Além da função `str_replace()`, existem outras opções para realizar a busca e substituição de textos em PHP, como a função `preg_replace()`, que permite o uso de expressões regulares. Além disso, é possível utilizar parâmetros adicionais como o número máximo de substituições e a partir de qual ocorrência a busca deve ser realizada.

No entanto, é importante ter cuidado com a sensibilidade a maiúsculas e minúsculas ao utilizar a função de busca e substituição, pois por padrão ela respeita a diferença entre letras maiúsculas e minúsculas.

## Veja também

- Documentação PHP sobre a função `str_replace()`: https://www.php.net/manual/pt_BR/function.str-replace.php
- Tutorial em vídeo sobre busca e substituição em PHP: https://www.youtube.com/watch?v=ePwH3uNpZuY
- Artigo sobre expressões regulares em busca e substituição em PHP: https://blog.schoolofnet.com/2018/10/busca-e-substituicao-em-php/