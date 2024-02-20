---
date: 2024-01-20 17:58:29.489867-07:00
description: "Buscar e substituir texto \xE9 o processo de localizar sequ\xEAncias\
  \ de caracteres dentro de uma string e alter\xE1-las por outras. Programadores fazem\
  \ isso para\u2026"
lastmod: 2024-02-19 22:05:05.744140
model: gpt-4-1106-preview
summary: "Buscar e substituir texto \xE9 o processo de localizar sequ\xEAncias de\
  \ caracteres dentro de uma string e alter\xE1-las por outras. Programadores fazem\
  \ isso para\u2026"
title: Pesquisando e substituindo texto
---

{{< edit_this_page >}}

## O Que é & Por Que?
Buscar e substituir texto é o processo de localizar sequências de caracteres dentro de uma string e alterá-las por outras. Programadores fazem isso para atualizar dados, corrigir erros, ou formatar informações de forma consistente.

## Como Fazer:
```Lua
local texto_original = "Lua é uma linguagem incrível!"
local texto_substituido = texto_original:gsub("incrível", "poderosa")

print(texto_substituido)  -- Saída: Lua é uma linguagem poderosa!
```

Além disso, podemos usar padrões mais complexos:
```Lua
local dados = "Nome: João, Idade: 30, Nome: Maria, Idade: 25"
local idade_atualizada = dados:gsub("Idade: (%d+)", function(idade)
  return "Idade: " .. (tonumber(idade) + 1)
end)

print(idade_atualizada)
-- Saída: Nome: João, Idade: 31, Nome: Maria, Idade: 26
```

## Mergulho Profundo
Buscar e substituir texto é uma função comum na maioria das linguagens de programação, e Lua não é exceção. Com a função `string.gsub`, Lua permite realizar substituições simples e também usar padrões complexos com expressões regulares, oferecendo grande flexibilidade.

Historicamente, tal funcionalidade é herança dos editores de texto e linguagens mais antigas, como SED em Unix, que lidavam fortemente com processamento de texto. Ao lidar com Lua, porém, é importante notar que usa-se um sistema de padrões (patterns) próprio, diferente das expressões regulares clássicas encontradas em outras linguagens.

Para tarefas mais complexas, pode-se recorrer a bibliotecas externas como o LPEG, que oferece um sistema de parsing de texto muito poderoso e flexível.

Detalhes de implementação:
- `gsub` retorna o novo texto e o número de substituições feitas.
- Padrões em Lua são mais simples que expressões regulares, mas suficientes para muitas tarefas.

## Veja Também
- [Referência da linguagem Lua 5.4 (em inglês)](https://www.lua.org/manual/5.4/)
- [Tutorial de padrões Lua (em inglês)](https://www.lua.org/pil/20.2.html)
- [LPEG - Biblioteca de Parsing de Lua (em inglês)](http://www.inf.puc-rio.br/~roberto/lpeg/lpeg.html)
