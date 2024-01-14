---
title:                "PHP: Gerando números aleatórios"
programming_language: "PHP"
category:             "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/php/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Por que gerar números aleatórios em programação?

Gerar números aleatórios é uma tarefa muito comum em programação, especialmente em jogos, sorteios e aplicações que requerem elementos imprevisíveis. Isso permite adicionar uma dose de imprevisibilidade e aleatoriedade às suas aplicações, tornando-as mais divertidas e interessantes para os usuários.

## Como gerar números aleatórios em PHP

Gerar números aleatórios em PHP é uma tarefa simples e fácil. A linguagem possui uma função embutida chamada `rand()`, que pode ser usada para gerar números aleatórios dentro de um determinado intervalo.

Vamos dar uma olhada em um exemplo:

```PHP
<?php
// Gerando um número aleatório entre 1 e 10
$randomNumber = rand(1, 10);

// Imprimindo o número gerado
echo "O número aleatório é: " . $randomNumber;
```

A saída desse exemplo pode ser algo como:

```
O número aleatório é: 4
```

Você também pode utilizar outras funções, como `mt_rand()` e `random_int()`, para gerar números aleatórios mais seguros e imprevisíveis.

## Mergulho Profundo: Detalhes sobre a geração de números aleatórios

Embora seja uma tarefa simples em PHP, a geração de números aleatórios é uma atividade complexa por trás dos bastidores. Na verdade, não existem números verdadeiramente aleatórios em programação. Em vez disso, são utilizados algoritmos matemáticos que, juntamente com uma "semente" (um número de partida), geram uma sequência imprevisível de números.

Além disso, é importante notar que a qualidade dos números gerados depende do algoritmo utilizado e da habilidade do programador em escolher adequadamente a semente e manipular os resultados da forma desejada.

## Veja também

- Documentação Oficial do PHP: [Gerar números aleatórios](https://www.php.net/manual/pt_BR/function.rand.php)
- Tutorial da DigitalOcean: [Como gerar números aleatórios em PHP](https://www.digitalocean.com/community/tutorials/how-to-generate-random-numbers-in-php)
- Artigo da GoHacking: [Segurança na geração de números aleatórios em PHP](https://gohacking.com/php-random-number-generator-security/)