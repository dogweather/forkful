---
title:                "Buscando e substituindo texto"
html_title:           "PHP: Buscando e substituindo texto"
simple_title:         "Buscando e substituindo texto"
programming_language: "PHP"
category:             "PHP"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/php/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## O que e Por que?

Substituir e encontrar textos é uma tarefa comum realizada por programadores ao trabalhar com código. Essa ação envolve a busca por um determinado texto ou padrão e sua substituição por outro. Os programadores fazem isso para corrigir erros, atualizar informações, limpar o código ou adicionar novas funcionalidades.

## Como fazer:

As funções "str_replace" e "preg_replace" permitem que os programadores substituam e encontrem texto em PHP.

```PHP
// Example using str_replace
$texto = "Olá mundo!";
$substituto = "Hello world!";
$resultado = str_replace("Olá", "Hello", $texto);

echo $resultado; // Output: Hello mundo!

// Example using preg_replace
$texto = "Eu tenho 30 anos";
$substituto = "tenho 30 anos";
$resultado = preg_replace("/Eu /", "", $texto);

echo $resultado; // Output: tenho 30 anos
```

## Profundidade:

A necessidade de substituir e encontrar texto surge em muitas áreas do desenvolvimento de software. Por exemplo, em editores de texto, é possível pesquisar e substituir palavras em todo o documento. Em PHP, as funções str_replace e preg_replace são as principais opções para essa tarefa. No entanto, também é possível usar regex (expressões regulares) para encontrar e manipular texto de forma mais específica.

## Veja também:

- [Documentação PHP para str_replace](https://www.php.net/manual/pt_BR/function.str-replace.php)
- [Documentação PHP para preg_replace](https://www.php.net/manual/pt_BR/function.preg-replace.php)
- [Expressões Regulares em PHP](https://www.php.net/manual/pt_BR/reference.pcre.pattern.syntax.php)