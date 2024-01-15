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

## Por que 

Frequentemente, ao escrevermos códigos em PHP, nos deparamos com a necessidade de procurar e substituir determinados trechos de texto. Isso pode ser devido a erros de digitação, alterações de requisitos ou simplesmente para fazer uma mudança em massa em todo o código. A busca e substituição de texto é uma tarefa comum na programação e, felizmente, o PHP possui algumas funções que facilitam esse processo.

## Como Fazer 

A função `str_replace()` é uma das maneiras mais simples de buscar e substituir texto em PHP. Ela possui três parâmetros obrigatórios: a string que será procurada, a string que será usada como substituição e a string em que a substituição será feita. Veja um exemplo de código:

```PHP
<?php
$texto = "Olá mundo!";
echo str_replace("mundo", "Pessoal", $texto);
```

Neste exemplo, a função `str_replace()` busca pela palavra "mundo" na string e a substitui por "Pessoal". O resultado impresso será "Olá Pessoal!".

É importante mencionar que a função `str_replace()` faz a substituição de todas as ocorrências do texto que está sendo buscado. Se você quiser substituir apenas a primeira ocorrência, pode usar a função `preg_replace()` em conjunto com uma expressão regular. Veja outro exemplo:

```PHP
<?php
$texto = "Este é um texto de teste para buscar e substituir.";
echo preg_replace("/test/", "exemplo", $texto, 1);
```

Neste caso, a função `preg_replace()` busca pela palavra "test" na string e a substitui por "exemplo", mas apenas na primeira ocorrência. O resultado impresso será "Este é um texto de exemplo para buscar e substituir.".

## Mergulho Profundo 

Além da função `str_replace()` e `preg_replace()`, o PHP também possui outras opções para buscar e substituir texto, como as funções `str_ireplace()` (que é case-insensitive) e `substr_replace()` (que permite substituir apenas parte de uma string). Além disso, existem bibliotecas externas disponíveis, como o pacote Regex, que oferece recursos avançados para manipulação de texto com expressões regulares.

Uma dica importante é sempre verificar a documentação oficial do PHP para entender melhor como cada função de busca e substituição funciona e quais parâmetros elas aceitam. Compreender bem essas funções pode economizar tempo e evitar possíveis erros no seu código.

## Veja Também 

Para mais informações sobre como buscar e substituir texto em PHP, confira os seguintes links:

- Documentação oficial do PHP: https://www.php.net/manual/pt_BR/function.str-replace.php
- Pacote Regex: https://regex.php.net/