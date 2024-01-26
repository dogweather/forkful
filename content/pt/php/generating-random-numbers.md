---
title:                "Gerando números aleatórios"
date:                  2024-01-20T17:49:30.146455-07:00
model:                 gpt-4-1106-preview
simple_title:         "Gerando números aleatórios"
programming_language: "PHP"
category:             "PHP"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/php/generating-random-numbers.md"
---

{{< edit_this_page >}}

## O quê & Por quê?
Gerar números aleatórios é como rolar um dado virtual: você nunca sabe o número que vai sair. Programadores usam essa técnica para tudo, desde criar dados de teste até alimentar algoritmos de criptografia.

## Como fazer:
```PHP
<?php
// Gerar um número inteiro aleatório entre 0 e 100
$numeroAleatorio = rand(0, 100);
echo $numeroAleatorio;

// Gerar um número mais seguro para criptografia
$numeroCrypto = random_int(0, 100);
echo $numeroCrypto;
?>
```

Pode sair algo assim:
```
42
76
```

## Aprofundamento
No passado, a função `rand()` era o padrão para gerar números aleatórios em PHP, mas ela não era boa para criptografia por ser previsível. Com a introdução do PHP 7, `random_int()` chegou para oferecer uma opção compatível com criptografia, utilizando fontes mais seguras de aleatoriedade. Além dessas funções, `mt_rand()` é uma alternativa que usa o algoritmo "Mersenne Twister" para ser mais rápido e fornecer melhor distribuição dos números.

## Ver também
- [PHP Manual - Function Reference - Math](https://www.php.net/manual/en/ref.math.php)
- [PHP Manual - `rand()` Function](https://www.php.net/manual/en/function.rand.php)
- [PHP Manual - `random_int()` Function](https://www.php.net/manual/en/function.random-int.php)
- [PHP Manual - `mt_rand()` Function](https://www.php.net/manual/en/function.mt-rand.php)
- [OpenSSL for PHP - Cryptographically secure pseudo-random numbers](https://www.php.net/manual/en/book.openssl.php)
