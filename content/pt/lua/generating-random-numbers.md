---
title:                "Gerando números aleatórios"
html_title:           "Lua: Gerando números aleatórios"
simple_title:         "Gerando números aleatórios"
programming_language: "Lua"
category:             "Lua"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/lua/generating-random-numbers.md"
---

{{< edit_this_page >}}

## O que & Por quê?

Gerar números aleatórios é uma forma de produzir números que são imprevisíveis e não seguem um padrão específico. Isso é útil para criar variedade em programas e jogos, ou para testar algoritmos. Programadores usam a geração de números aleatórios para adicionar um elemento de aleatoriedade e imprevisibilidade em seus projetos.

## Como fazer:

```
-- Exemplo 1: Gerar um número inteiro entre 1 e 10
math.randomseed(os.time()) -- Define a semente de acordo com o tempo atual para evitar a repetição de resultados
print(math.random(1,10)) -- Imprime um número entre 1 e 10 (pode variar a cada execução)

-- Exemplo 2: Gerar um número decimal entre 0 e 1
math.randomseed(os.time())
print(math.random()) -- Imprime um número decimal entre 0 e 1 (pode variar a cada execução)

```

## Aprofundando:

Existem diferentes algoritmos para a geração de números aleatórios, mas vale ressaltar que "aleatório" nesse contexto significa pseudorandom, ou seja, criado a partir de cálculos matemáticos que aparentam ser aleatórios. Alguns programadores utilizam bibliotecas externas para gerar números aleatórios, mas o Lua já possui a função math.random() que utiliza o algoritmo de congruente linear para gerar valores. Além disso, é possível definir uma semente, como no exemplo acima, para garantir que os resultados variem a cada execução.

## Veja também:

- [Documentação oficial do Lua sobre a geração de números aleatórios](https://www.lua.org/pil/20.2.html)
- [Artigo sobre geração de números aleatórios em linguagens de programação](https://www.ime.usp.br/~song/mac0499/031/gerando.html)
- [Vídeo explicando a diferença entre números aleatórios e pseudorandom](https://www.youtube.com/watch?v=u_hA3ApgBw8)