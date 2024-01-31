---
title:                "Geração de números aleatórios"
date:                  2024-01-27T20:34:51.317962-07:00
model:                 gpt-4-0125-preview
simple_title:         "Geração de números aleatórios"

category:             "PHP"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/php/generating-random-numbers.md"
---

{{< edit_this_page >}}

## O que & Por quê?

Gerar números aleatórios em PHP está relacionado a produzir valores imprevisíveis dentro de uma faixa especificada, o que é essencial para tarefas como criar IDs de usuário únicos, gerar senhas ou para uso em simulações e jogos. Programadores contam com a aleatoriedade para adicionar imprevisibilidade e variabilidade em suas aplicações, tornando processos como testes ou experiências de usuário mais robustos e envolventes.

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
