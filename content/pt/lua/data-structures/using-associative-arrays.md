---
changelog:
- 2024-01-30, gpt-4-0125-preview, translated from English
date: 2024-01-30 19:11:54.802832-07:00
description: "Como fazer: Em Lua, criar um array associativo (ou uma tabela, em linguagem\
  \ Lua) \xE9 simples. Voc\xEA abandona os \xEDndices num\xE9ricos usuais por chaves\
  \ de sua\u2026"
lastmod: '2024-03-13T22:44:46.702207-06:00'
model: gpt-4-0125-preview
summary: "Em Lua, criar um array associativo (ou uma tabela, em linguagem Lua) \xE9\
  \ simples."
title: Usando arrays associativos
weight: 15
---

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
