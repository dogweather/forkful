---
title:                "Gerando números aleatórios"
html_title:           "C: Gerando números aleatórios"
simple_title:         "Gerando números aleatórios"
programming_language: "Lua"
category:             "Lua"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/lua/generating-random-numbers.md"
---

{{< edit_this_page >}}

# Gerando Números Aleatórios em Lua

## O Quê & Porquê?

Gerar números aleatórios em programação é o ato de produzir sequências numéricas que não podem ser previstas logicamente. Isso é útil para uma gama de aplicações, desde jogos de azar até simulações científicas.

## Como Fazer:

Aqui estão algumas maneiras de como gerar números aleatórios no Lua. O exemplo a seguir mostra como você pode gerar um número aleatório entre 1 e 100.

```Lua
math.randomseed( os.time() )
local randomNumber = math.random(1,100)
print(randomNumber)
```

Ao executar este script, você verá um número aleatório impresso no terminal. 

## Mergulho Profundo

A função `math.random` do Lua tem uma longa história, seu comportamento original era dependente do sistema operacional, mas isso foi corrigido no Lua 5.3. Antes disso, a mesma sequência inicial de números era gerada cada vez que o programa era iniciado. Isso foi resolvido implementando a função `math.randomseed` que inicializa o gerador de números aleatórios.

Existem alternativas ao `math.random`. bibliotecas de terceiros, como a `randomlua`, oferecem implementações alternativas de geradores de números aleatórios. Essas bibliotecas podem ser úteis se você precisar de um comportamento mais complexo ou específico do que o Lua oferece por padrão.

Ao gerar números aleatórios, é importante levar em conta a distribuição. A função `math.random` do Lua irá gerar números com uma distribuição uniforme. Isso significa que cada número tem a mesma probabilidade de ser escolhido.

## Veja Também

Para mais detalhes sobre números aleatórios em Lua, você pode consultar a documentação oficial do Lua: [https://www.lua.org/manual/5.3/manual.html#6.9](https://www.lua.org/manual/5.3/manual.html#6.9)

Uma fonte alternativa é a documentação do `randomlua`: [https://github.com/soni-exe/randomlua](https://github.com/soni-exe/randomlua) 

Lembre-se, entender o comportamento dos números aleatórios e como gerá-los adequadamente é uma parte fundamental de ser um bom programador.