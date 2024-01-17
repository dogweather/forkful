---
title:                "Convertendo uma string para minúsculas"
html_title:           "PHP: Convertendo uma string para minúsculas"
simple_title:         "Convertendo uma string para minúsculas"
programming_language: "PHP"
category:             "PHP"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/php/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## O que e por que?

Converter uma string para letras minúsculas é um dos processos comuns na programação usando PHP. Isso significa transformar todas as letras maiúsculas em minúsculas em uma string. Isso pode ser útil quando se lida com entradas de usuários, onde pode haver inconsistências na capitalização do texto. Além disso, muitas funções e métodos do PHP aceitam apenas strings em letras minúsculas, então é necessário converter as strings para garantir que elas funcionem corretamente.

## Como fazer:

Existem várias maneiras de converter uma string para letras minúsculas em PHP. As duas funções principais são `strtolower()` e `mb_strtolower()`. A primeira é usada para strings em inglês, enquanto a segunda pode lidar com diferentes tipos de caracteres, como acentos e símbolos. Aqui está um exemplo de como usar ambas as funções:

```
$string = "Olá Mundo!";
echo strtolower($string); // saída: olá mundo!
echo mb_strtolower($string); // saída: olá mundo!
```

## Mergulho profundo:

A ideia de converter strings para letras minúsculas não é exclusiva do PHP. Na verdade, essa prática pode ser encontrada em diversas linguagens de programação. No entanto, pode haver variações na implementação, especialmente quando lida com diferentes conjuntos de caracteres e idiomas. Além das funções mencionadas acima, também é possível converter uma string para letras minúsculas usando métodos específicos de cada idioma, como `mb_convert_case()` para caracteres multibyte em PHP.

## Veja também:

- Documentação oficial do PHP sobre `strtolower()`: https://www.php.net/manual/en/function.strtolower.php
- Documentação oficial do PHP sobre `mb_strtolower()`: https://www.php.net/manual/en/function.mb-strtolower.php
- Documentação oficial do PHP sobre `mb_convert_case()`: https://www.php.net/manual/en/function.mb-convert-case.php