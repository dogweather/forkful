---
title:                "Excluindo caracteres que correspondem a um padrão"
html_title:           "Lua: Excluindo caracteres que correspondem a um padrão"
simple_title:         "Excluindo caracteres que correspondem a um padrão"
programming_language: "Lua"
category:             "Lua"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/lua/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## O quê e porquê?

Deletar caracteres que correspondem a um padrão é uma técnica comum utilizada por programadores para remover caracteres específicos de uma string. Isso pode ser útil para manipular dados de texto ou limpar informações desnecessárias antes de processá-las em um programa.

## Como fazer:

```Lua
-- Exemplo 1: Deletando todas as letras maiúsculas de uma string
local str = "Hello World!"
str = string.gsub(str, "%u", "")
print(str) -- Output: ello orld!

-- Exemplo 2: Deletando todos os números de uma string
local str = "Abc123xyz"
str = string.gsub(str, "%d", "")
print(str) -- Output: Abcxyz
```

## Investigação mais profunda:

Deletar caracteres que correspondem a um padrão é uma técnica que existe desde os primeiros dias da programação. Ela foi originalmente concebida para ser usada com expressões regulares, mas em Lua, usamos o padrão de string '%u' para corresponder a letras maiúsculas e '%d' para números. Isso pode ser uma alternativa mais simples ao uso de expressões regulares complexas.

## Veja também:

- Lista de padrões de string em Lua: http://www.lua.org/manual/5.3/manual.html#6.4.1
- Documentação de expressões regulares em Lua: http://www.lua.org/manual/5.3/manual.html#6.4.2