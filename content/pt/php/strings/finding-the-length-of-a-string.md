---
date: 2024-01-20 17:47:56.699910-07:00
description: "Encontrar o comprimento de uma string \xE9 simplesmente descobrir quantos\
  \ caracteres ela possui. Programadores fazem isso para validar dados, manipular\u2026"
lastmod: '2024-03-13T22:44:46.658536-06:00'
model: gpt-4-1106-preview
summary: "Encontrar o comprimento de uma string \xE9 simplesmente descobrir quantos\
  \ caracteres ela possui. Programadores fazem isso para validar dados, manipular\u2026"
title: Descobrindo o comprimento de uma string
---

{{< edit_this_page >}}

## O Quê & Porquê?
Encontrar o comprimento de uma string é simplesmente descobrir quantos caracteres ela possui. Programadores fazem isso para validar dados, manipular texto, e onde a precisão do tamanho é crucial, como em protocolos de comunicação ou armazenamento de dados.

## Como fazer:
```PHP
<?php
$texto = "Olá, mundo!";
$comprimento = strlen($texto);

echo $comprimento; // Saída: 12
```
Note que `strlen` conta caracteres, então se o texto contiver caracteres especiais ou acentuados, os resultados podem variar dependendo da codificação.

## Mergulho Profundo
Historicamente, `strlen` tem sido a função padrão no PHP para medir o comprimento de uma string desde sua concepção. Com o tempo e evolução das codificações, como UTF-8, a contagem de caracteres tornou-se menos direta devido à variedade na quantidade de bytes que um caractere pode ocupar.

Alternativas como `mb_strlen` surgiram para lidar melhor com múltiplos idiomas e codificações. Se você está trabalhando com UTF-8, `mb_strlen` é a escolha correta para obter um resultado preciso.

```PHP
<?php
$texto = "Olá, mundo!";
$comprimento = mb_strlen($texto, 'UTF-8');

echo $comprimento; // Saída será: 10
```

Quando você escolhe `mb_strlen` em vez de `strlen`, você está considerando uma abordagem mais global e inclusiva à diversidade de idiomas na programação.

## Veja Também
- [Documentação oficial do PHP sobre strlen](https://www.php.net/manual/pt_BR/function.strlen.php)
- [Documentação oficial do PHP sobre mb_strlen](https://www.php.net/manual/pt_BR/function.mb-strlen.php)
- [Explicação sobre codificação de caracteres UTF-8](https://pt.wikipedia.org/wiki/UTF-8)
