---
title:                "Gerando números aleatórios"
html_title:           "C: Gerando números aleatórios"
simple_title:         "Gerando números aleatórios"
programming_language: "PHP"
category:             "PHP"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/php/generating-random-numbers.md"
---

{{< edit_this_page >}}

## O Que & Por Quê?

Gerar números aleatórios em programação é a criação de valores numéricos que não têm padrão ou correlação previsível. Programadores geram números aleatórios para criar cenários variados e imprevisíveis em jogos, testes de simulação e outros contextos.

## Como Fazer:

Está se sentindo sortudo? Vamos correr alguns exemplos simples utilizando a função `rand()` e `mt_rand()` do PHP, e o novo método `random_int()` introduzido no PHP 7.

```PHP
<?php
// Usando a função rand()
$random_num = rand(1, 100);
echo $random_num;
?>
```

```PHP
<?php
// Usando a função mt_rand()
$random_num = mt_rand(1, 100);
echo $random_num;
?>
```

```PHP
<?php
// Usando a função random_int()
try {
    $random_num = random_int(1, 100);
} catch (Exception $e) {
    // Conduzir lógica de tratamento de exceções aqui.
}
echo $random_num;
?>
```

A saída de cada um desses scripts será um número aleatório entre 1 e 100.

## Mergulho Profundo:

O PHP, ao longo de sua história, sempre forneceu uma forma de gerar números aleatórios. No início, tínhamos apenas a função `rand()`, mas como ela não era boa o suficiente para algumas aplicações, a função `mt_rand()` foi introduzida. Esta última, baseada no algoritmo Mersenne Twister, gera números aleatórios de uma maneira insuperável, oferecendo uma combinação de velocidade e qualidade que até mesmo `rand()` não poderia alcançar.

Na versão 7.x do PHP, a função `random_int()` foi introduzida. Esta função gera verdadeiros números aleatórios inteiros numa gama especificada de forma segura. Isto é extremamente importante para aplicações onde a segurança e a imprevisibilidade são necessárias, tais como na geração de tokens de segurança.

Como alternativas, existem bibliotecas PHP que fornecem mais funcionalidades para gerar números aleatórios, como o RandomLib e o SecureRandom, que fornecem APIs adicionais e abstração para geração de números aleatórios em vários ambientes.

## Veja Também:

Quer entrar ainda mais fundo no assunto? São links úteis:

- [PHP Manual on Random Functions](https://www.php.net/manual/en/book.math.php)
- [PHP Manual on Cryptography Extensions](https://www.php.net/manual/en/book.crypt.php)
- [RandomLib on GitHub](https://github.com/ircmaxell/random-lib)
- [SecureRandom on GitHub](https://github.com/paragonie/random_compat)