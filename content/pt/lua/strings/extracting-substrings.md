---
date: 2024-01-20 17:46:19.840973-07:00
description: 'Como Fazer: .'
lastmod: '2024-03-13T22:44:46.698505-06:00'
model: gpt-4-1106-preview
summary: .
title: Extraindo substrings
weight: 6
---

## Como Fazer:
```Lua
local frase = "Olá, programadores Lua!"
-- Extraindo substring usando string.sub
local saudacao = string.sub(frase, 1, 4)
print(saudacao) -- Saída: Olá,

-- Extraindo substrings entre posições específicas
local grupo = string.sub(frase, 6, 20)
print(grupo) -- Saída: programadores

-- Extraindo até o final da string sem definir o segundo parâmetro
local resto = string.sub(frase, 22)
print(resto) -- Saída: Lua!
```

## Mergulho Profundo:
Historicamente, no mundo da programação, extrair substrings se mostrou essencial para tratamento de texto. No Lua, a função `string.sub` é versátil e usa índices baseados em 1, ao contrário de outras linguagens como C, que são baseadas em 0. Isso pode confundir um pouco se você está acostumado com outras linguagens, mas faz todo o sentido dentro da filosofia do Lua de ser amigável.

Alternativas para funções de extração existem. Você pode encontrar a função `string.match`, que permite extrair substrings usando padrões (pattern matching). Já a função `string.gmatch` é um gerador, perfeita para iterar sobre todas as partes que correspondem ao padrão dentro da string.

Detalhes de implementação são relevantes, como saber que a extração de strings no Lua é uma operação segura. Se você especificar um índice que está fora da string, `string.sub` ajustará automaticamente para o tamanho correto. Essa é uma camada extra de segurança para evitar erros durante o corte de suas strings.
