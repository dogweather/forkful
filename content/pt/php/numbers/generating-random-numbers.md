---
date: 2024-01-27 20:34:51.317962-07:00
description: "Como: O PHP oferece v\xE1rias fun\xE7\xF5es para gerar n\xFAmeros aleat\xF3\
  rios, mas as mais usadas s\xE3o `rand()`, `mt_rand()`, e para prop\xF3sitos criptogr\xE1\
  ficos,\u2026"
lastmod: '2024-03-13T22:44:46.663228-06:00'
model: gpt-4-0125-preview
summary: "O PHP oferece v\xE1rias fun\xE7\xF5es para gerar n\xFAmeros aleat\xF3rios,\
  \ mas as mais usadas s\xE3o `rand()`, `mt_rand()`, e para prop\xF3sitos criptogr\xE1\
  ficos, `random_int()`."
title: "Gera\xE7\xE3o de n\xFAmeros aleat\xF3rios"
weight: 12
---

## Como:
O PHP oferece várias funções para gerar números aleatórios, mas as mais usadas são `rand()`, `mt_rand()`, e para propósitos criptográficos, `random_int()`.

Para gerar um número aleatório simples entre 0 e getrandmax() (o maior valor possível retornado por `rand()`), você pode usar:

```PHP
echo rand();
```

Para uma faixa mais específica, como entre 1 e 100:

```PHP
echo rand(1, 100);
```

No entanto, `mt_rand()` é uma melhor escolha para velocidade e aleatoriedade:

```PHP
echo mt_rand(1, 100);
```

A saída para ambos pode ser qualquer coisa entre 1 e 100, dependendo da randomização, por exemplo, `42`.

Para contextos criptográficos ou de segurança, onde a imprevisibilidade é crucial, `random_int()` é a escolha preferida, pois gera inteiros pseudo-aleatórios criptograficamente seguros:

```PHP
echo random_int(1, 100);
```

Novamente, a saída é um número aleatório entre 1 e 100, como `84`, mas com uma garantia mais forte de aleatoriedade.

## Aprofundamento
A função `rand()` está presente no PHP desde suas versões iniciais, servindo como a abordagem inicial para gerar números aleatórios. No entanto, não é a melhor escolha para aplicações que requerem um alto grau de aleatoriedade devido ao seu algoritmo relativamente previsível.

`mt_rand()`, introduzido no PHP 4, é baseado no algoritmo Mersenne Twister - muito superior em termos de velocidade e da aleatoriedade que pode gerar em comparação a `rand()`. Rapidamente se tornou a opção preferida para a maioria das necessidades não criptográficas.

Para aplicações sensíveis à segurança, `random_int()` foi introduzido no PHP 7 para gerar inteiros pseudo-aleatórios criptograficamente seguros usando bytes aleatórios do gerador de números aleatórios do sistema. É significativamente mais seguro que `rand()` ou `mt_rand()`, tornando-o a melhor escolha para gerar tokens, chaves ou outros elementos onde a previsibilidade poderia levar a vulnerabilidades de segurança.

Apesar dessas melhorias, é crucial escolher a função certa com base no contexto da aplicação. Para uso geral, `mt_rand()` é suficiente, mas para qualquer coisa que possa ser alvo de exploração, `random_int()` é o caminho a seguir, fornecendo tanto aleatoriedade quanto segurança.
