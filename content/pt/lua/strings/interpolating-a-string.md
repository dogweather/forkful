---
date: 2024-01-20 17:51:08.674992-07:00
description: "Como Fazer: Historicamente, a interpola\xE7\xE3o de strings era comum\
  \ em muitas linguagens, e no Lua, a fun\xE7\xE3o `string.format` \xE9 inspirada\
  \ no `printf` da\u2026"
lastmod: '2024-04-05T21:53:47.040930-06:00'
model: gpt-4-1106-preview
summary: "Historicamente, a interpola\xE7\xE3o de strings era comum em muitas linguagens,\
  \ e no Lua, a fun\xE7\xE3o `string.format` \xE9 inspirada no `printf` da linguagem\
  \ C."
title: Interpolando uma string
weight: 8
---

## Como Fazer:
```Lua
-- Exemplo básico de interpolação de string usando a função string.format
local nome = "João"
local idade = 28
local mensagem = string.format("Olá, %s! Você tem %d anos.", nome, idade)
print(mensagem)  -- Saída: Olá, João! Você tem 28 anos.

-- Interpolação complexa com múltiplos tipos de dados
local preco = 49.99
local quantidade = 3
local frase = string.format("O total da sua compra de %d itens é R$ %.2f", quantidade, preco * quantidade)
print(frase)  -- Saída: O total da sua compra de 3 itens é R$ 149.97
```

## Aprofundamento
Historicamente, a interpolação de strings era comum em muitas linguagens, e no Lua, a função `string.format` é inspirada no `printf` da linguagem C. Em Lua, não há suporte nativo para a interpolação de string sintática, como você pode encontrar em Ruby ou Python com suas strings interpoladas ou "f-strings", respectivamente. No entanto, usar `string.format` é um método poderoso e flexível que permite não apenas inserir variáveis dentro de uma string, mas também formatar números e datas, por exemplo, com controle específico sobre a precisão e o formato de exibição. Alternativas incluem a concatenação manual de strings ou o uso de bibliotecas externas que podem oferecer uma sintaxe mais concisa para interpolação.
