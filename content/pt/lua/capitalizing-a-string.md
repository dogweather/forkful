---
title:                "Maiúscula de uma string"
html_title:           "Lua: Maiúscula de uma string"
simple_title:         "Maiúscula de uma string"
programming_language: "Lua"
category:             "Lua"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/lua/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## O que e por que?

Capitalizar uma string em Lua significa transformar todas as letras minúsculas em maiúsculas. Os programadores fazem isso para padronizar a formatação de texto e facilitar a comparação de strings.

## Como fazer:

```lua
-- Exemplo 1: Usando a função string.upper()
local texto = "exemplo de texto"
print(string.upper(texto))
-- Saída: EXEMPLO DE TEXTO

-- Exemplo 2: Usando um laço de repetição
local texto = "exemplo de texto"
local novoTexto = ""
for i = 1, #texto do
  local letra = string.sub(texto, i, i)
  if string.byte(letra) >= 97 and string.byte(letra) <= 122 then
    letra = string.char(string.byte(letra) - 32)
  end
  novoTexto = novoTexto .. letra
end
print(novoTexto)
-- Saída: EXEMPLO DE TEXTO
```

## Profundando:

Antigamente, a função string.upper() não estava disponível em Lua, então os programadores precisavam usar o método manual mostrado no exemplo 2.

Além da função string.upper(), também existe a função string.lower() que transforma todas as letras em minúsculas.

## Veja também:

- Documentação oficial do Lua sobre strings: https://www.lua.org/manual/5.4/manual.html#6.4.1
- Página sobre Lua na Wikipedia: https://pt.wikipedia.org/wiki/Lua_(linguagem_de_programa%C3%A7%C3%A3o)