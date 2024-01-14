---
title:                "PHP: Encontrando o tamanho de uma string"
simple_title:         "Encontrando o tamanho de uma string"
programming_language: "PHP"
category:             "PHP"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/php/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Por que

Encontrar o comprimento de uma string é uma tarefa comum em programação, pois muitas vezes precisamos saber quantos caracteres uma palavra ou frase possui. Além disso, entender como funciona o cálculo do comprimento de uma string pode nos ajudar a ter uma melhor compreensão sobre manipulação de dados em PHP.

## Como Fazer

Para encontrar o comprimento de uma string em PHP, podemos usar a função "strlen()". Veja abaixo um exemplo de como utilizá-la em código PHP e o resultado esperado:

```PHP
<?php

$string = "Olá, leitores!";

echo strlen($string);  // saída: 14 (contando espaços e pontuações)
```

Neste exemplo, declaramos uma variável "string" com a palavra "Olá, leitores!" e usamos a função "strlen()" para encontrar seu comprimento. Ao executar o código, a saída será 14, já que a função também conta os espaços e pontuações.

Também é possível utilizar outras funções como "mb_strlen()" para lidar com strings multibyte ou "mb_strlen_count()" para evitar problemas com caracteres UTF-8. É importante entender bem o funcionamento dessas funções para evitar erros em seus projetos.

## Deep Dive

Por trás do cálculo do comprimento de uma string, há um conceito importante: índices. Em PHP, cada caractere de uma string tem um índice, começando de 0 para o primeiro caractere. Portanto, se quisermos acessar um caractere específico de uma string, podemos usar seu índice correspondente. Por exemplo, para acessar a letra "l" da palavra "Olá", usamos o índice 1 (lembrando que o primeiro caractere é indexado como 0).

Além disso, é importante saber que o comprimento de uma string é o número total de índices. Por exemplo, a string "Olá" possui 3 caracteres, mas seu comprimento é 4, já que o índice começa em 0 e vai até 3.

## Veja Também

- [Função strlen() no Manual do PHP](https://www.php.net/manual/pt_BR/function.strlen.php)
- [Função mb_strlen() no Manual do PHP](https://www.php.net/manual/pt_BR/function.mb-strlen.php)
- [Função mb_strlen_count() no Manual do PHP](https://www.php.net/manual/pt_BR/function.mb-strlen-count.php)