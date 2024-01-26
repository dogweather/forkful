---
title:                "Gerando números aleatórios"
date:                  2024-01-20T17:49:43.784624-07:00
model:                 gpt-4-1106-preview
simple_title:         "Gerando números aleatórios"
programming_language: "Lua"
category:             "Lua"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/lua/generating-random-numbers.md"
---

{{< edit_this_page >}}

## What & Why?
Gerar números aleatórios é o processo de criação de valores imprevisíveis. Programadores fazem isso por diversos motivos, desde jogos e sorteios até simulações e segurança da informação.

## How to:
Em Lua, você pode gerar números aleatórios facilmente com as funções `math.randomseed` e `math.random`. Primeiro, definimos uma "semente" para inicializar o gerador:

```Lua
math.randomseed(os.time())
```

Depois, geramos números aleatórios dentro de um intervalo:

```Lua
-- Número aleatório entre 1 e 10
local numero = math.random(1, 10)
print(numero)
```

Se não passar argumentos, `math.random()` retorna um número aleatório entre 0 e 1.

```Lua
-- Número aleatório entre 0 e 1
local flutuante = math.random()
print(flutuante)
```

## Deep Dive
A geração de números aleatórios em computação nem sempre é "pura" – geralmente são pseudoraleatórios, baseados em algoritmos determinísticos. Em Lua, a "seed" ou semente, que geralmente usamos com a função `os.time()`, permite iniciar o gerador de números de modo que os resultados sejam diferentes a cada execução.

Além da biblioteca padrão, há alternativas como a `love.math`, se você estiver usando o framework LÖVE para desenvolvimento de jogos, que proporciona funções adicionais de randomização.

Quanto à implementação, Lua utiliza a função `rand()` da biblioteca C, que está subjacente ao `math.random`. A distribuição é, na maioria das vezes, uniforme, mas isso pode variar segundo a plataforma.

## See Also
- Documentação oficial de Lua sobre a biblioteca de matemática: https://www.lua.org/manual/5.4/manual.html#6.7
- Artigo sobre números pseudoraleatórios: https://pt.wikipedia.org/wiki/Gerador_de_números_pseudoaleatórios
- Framework LÖVE para mais funções relacionadas a randomização: https://love2d.org/wiki/love.math
