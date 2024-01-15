---
title:                "Excluindo caracteres que correspondem a um padrão"
html_title:           "PHP: Excluindo caracteres que correspondem a um padrão"
simple_title:         "Excluindo caracteres que correspondem a um padrão"
programming_language: "PHP"
category:             "PHP"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/php/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Por que

Às vezes, em nossos códigos PHP, nos deparamos com caracteres que não queremos manter. Eles podem ser espaços em branco, símbolos ou até mesmo palavras inteiras. Para evitar problemas ou erros, é importante saber como deletar esses caracteres de uma maneira eficiente.

## Como fazer

Felizmente, o PHP possui uma função integrada que nos permite facilmente deletar caracteres indesejados. A função `preg_replace()` permite que substituamos caracteres que correspondam a um padrão específico por um novo conteúdo. Por exemplo:

```
<?php
$string = "Esta é uma frase com caracteres indesejados   ";
echo preg_replace("/\s+/", "", $string);
?>
```

Este código irá deletar todos os espaços em branco da string, resultando em "Estaéumafrasecomcaracteresindesejados". Observe que o primeiro parâmetro da função é uma expressão regular, que nos permite definir o padrão a ser encontrado.

## Deep Dive

Expressões regulares são uma ferramenta poderosa para manipulação de strings em PHP. Existem diversos padrões que podem ser utilizados para encontrar e deletar caracteres específicos. Além disso, a função `preg_replace()` também aceita arrays como parâmetros, o que nos permite deletar vários caracteres em uma única chamada de função.

Outra opção é utilizar a função `str_replace()`, que permite a substituição de um conjunto de caracteres por outro conjunto. Essa função é mais simples de usar, mas pode não oferecer a flexibilidade da `preg_replace()`.

Por fim, é importante lembrar que ambas as funções mencionadas acima não alteram a string original, apenas retornam o resultado da operação. Portanto, é necessário atribuir o resultado a uma variável ou imprimir diretamente.

## Veja também

- [Documentação oficial do PHP sobre a função preg_replace()](https://www.php.net/manual/pt_BR/function.preg-replace.php)
- [Tutorial sobre expressões regulares em PHP](https://www.php.net/manual/pt_BR/book.pcre.php)