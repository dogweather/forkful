---
title:                "PHP: Extraindo subcadeias de caracteres"
simple_title:         "Extraindo subcadeias de caracteres"
programming_language: "PHP"
category:             "PHP"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/php/extracting-substrings.md"
---

{{< edit_this_page >}}

## Por que extrair substrings em PHP?

Extrair substrings de uma string é uma tarefa comum em programação, especialmente em PHP. Pode ser necessário para realizar a manipulação de dados, análise de texto ou até mesmo para segurança. Aprender a extrair substrings em PHP pode ser útil em várias situações e é uma habilidade essencial para qualquer programador PHP.

## Como fazer isso em PHP?

Para extrair substrings em PHP, você precisará usar a função `substr()`. Essa função aceita três parâmetros: uma string, a posição inicial onde deseja começar a extrair e o comprimento do substring desejado. Veja um exemplo de código abaixo:

```PHP
$string = "Olá, mundo!";
$subtring = substr($string, 5, 5);
echo $substring; // resultado: mundo
```

Neste exemplo, começamos a extrair a partir do quinto caractere e extraímos um substring com cinco caracteres de comprimento. Você também pode usar valores negativos para indicar a posição contando de trás para frente.

## Mergulho mais profundo na extração de substrings

Além da função `substr()`, também existem outras funções úteis para extrair substrings em PHP, como `mb_substr()` e `str_split()`. Além disso, você pode usar expressões regulares para realizar a extração em casos mais complexos.

Lembre-se de que a posição inicial e o comprimento do substring podem ser variáveis, permitindo que você crie lógicas mais dinâmicas em seu código. Além disso, é importante considerar a codificação da string ao realizar a extração, pois isso pode afetar os resultados.

## Veja também

- [Documentação da função substr() em PHP](https://www.php.net/manual/pt_BR/function.substr.php)
- [Documentação da função mb_substr() em PHP](https://www.php.net/manual/pt_BR/function.mb-substr.php)
- [Documentação da função str_split() em PHP](https://www.php.net/manual/pt_BR/function.str-split.php)