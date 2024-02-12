---
title:                "Usando arrays associativos"
aliases:
- /pt/lua/using-associative-arrays.md
date:                  2024-01-30T19:11:54.802832-07:00
model:                 gpt-4-0125-preview
simple_title:         "Usando arrays associativos"

tag:                  "Data Structures"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/lua/using-associative-arrays.md"
changelog:
  - 2024-01-30, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## O Que & Por Que?

Arrays associativos são como apertos de mão secretos para dados em Lua—em vez de apenas números se alinhando obedientemente por índice, suas chaves podem ser o que você quiser, tornando a recuperação de dados uma brisa. Por que os programadores os usam? Porque, às vezes, você precisa chamar um pedaço de dados pelo seu nome, e não por um número de ordem.

## Como fazer:

Em Lua, criar um array associativo (ou uma tabela, em linguagem Lua) é simples. Você abandona os índices numéricos usuais por chaves de sua própria escolha. Veja só:

```Lua
-- Criando um array associativo
userInfo = {
  name = "Jamie",
  occupation = "Aventureiro",
  level = 42
}

-- Acessando elementos
print(userInfo["name"]) -- Imprime Jamie
print(userInfo.occupation) -- Imprime Aventureiro

-- Adicionando novos pares chave-valor
userInfo["hobby"] = "Programar"
userInfo.favLang = "Lua"

-- Iterando sobre o array associativo
for key, value in pairs(userInfo) do
  print(key .. ": " .. value)
end
```

Saída:
```
Jamie
Aventureiro
name: Jamie
occupation: Aventureiro
level: 42
hobby: Programar
favLang: Lua
```

A parte legal? Você interage com os dados usando chaves significativas para você, tornando o código mais legível e fácil de manter.

## Mergulho Profundo

Quando Lua entrou em cena, introduziu tabelas como uma estrutura de dados para todos os usos, revolucionando como os desenvolvedores gerenciam dados. Diferentemente de algumas linguagens onde arrays associativos e arrays são entidades distintas, as tabelas de Lua servem como ambos, simplificando a paisagem da estrutura de dados.

O que torna as tabelas de Lua particularmente poderosas é sua flexibilidade. No entanto, esta flexibilidade vem com o custo de potenciais implicações de desempenho, especialmente com grandes conjuntos de dados onde uma estrutura de dados mais especializada pode ser preferível para eficiência.

Embora Lua não suporte nativamente estruturas de dados mais convencionais prontamente, como listas ligadas ou mapas de hash, a adaptabilidade da estrutura de tabela significa que você pode implementar essas usando tabelas se precisar. Só lembre: com grande poder vem grande responsabilidade. Use a flexibilidade com sabedoria para manter o desempenho e a legibilidade do seu código.
