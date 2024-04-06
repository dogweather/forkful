---
date: 2024-01-20 17:42:34.044073-07:00
description: "Como fazer: O poder das express\xF5es regulares e padr\xF5es em Lua\
  \ come\xE7ou a se tornar indispens\xE1vel na era do processamento de texto avan\xE7\
  ado. A fun\xE7\xE3o\u2026"
lastmod: '2024-04-05T21:53:47.038916-06:00'
model: gpt-4-1106-preview
summary: "O poder das express\xF5es regulares e padr\xF5es em Lua come\xE7ou a se\
  \ tornar indispens\xE1vel na era do processamento de texto avan\xE7ado."
title: "Excluindo caracteres que correspondem a um padr\xE3o"
weight: 5
---

## Como fazer:
```lua
local texto = "O rato roeu a roupa do rei de Roma 123."
local padrao = "%d" -- isso significa qualquer dígito decimal

-- Substituir dígitos por uma string vazia para deletá-los
local resultado = texto:gsub(padrao, "")
print(resultado) -- Saída: O rato roeu a roupa do rei de Roma .

-- Outro exemplo: Remover espaços
padrao = "%s" -- isso representa espaços em branco
resultado = texto:gsub(padrao, "")
print(resultado) -- Saída: OratoroeuaroupadoreideRoma123.
```

## Aprofundando
O poder das expressões regulares e padrões em Lua começou a se tornar indispensável na era do processamento de texto avançado. A função `string.gsub` não usa expressões regulares completas como em outras linguagens, mas os padrões de Lua, que são suficientes para muitas tarefas de manipulação de strings. Alternativas, como as funções `string.find` e `string.match`, podem ser usadas para detectar padrões sem substituí-los, enquanto bibliotecas externas podem oferecer expressões regulares plenas se necessário. 

Os detalhes de implementação da função `gsub` envolvem o uso de padrões, que incluem conjunto de caracteres, representantes, e modificadores. Por exemplo, `%d` representa qualquer numeral, enquanto `%s` representa qualquer espaço em branco. Para entender melhor esses padrões, é essencial ler a documentação oficial e praticar.

## Veja Também
- Documentação oficial do Lua sobre padrões: https://www.lua.org/pil/20.2.html
- Tutorial de Lua para iniciantes: https://learnxinyminutes.com/docs/pt-br/lua-pt/
- Referência da biblioteca de strings do Lua: https://www.lua.org/manual/5.4/manual.html#6.4
