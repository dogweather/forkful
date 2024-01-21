---
title:                "Excluindo caracteres que correspondem a um padrão"
date:                  2024-01-20T17:42:34.044073-07:00
model:                 gpt-4-1106-preview
simple_title:         "Excluindo caracteres que correspondem a um padrão"
programming_language: "Lua"
category:             "Lua"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/lua/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## O Quê e Por Quê?
Deletar caracteres que correspondem a um padrão é basicamente filtrar texto, removendo partes específicas dele. Programadores fazem isso para limpeza de dados, validação de entrada ou simples manipulação de strings quando os dados precisam de um formato específico.

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