---
title:                "Convertendo uma string para minúsculas"
html_title:           "Fish Shell: Convertendo uma string para minúsculas"
simple_title:         "Convertendo uma string para minúsculas"
programming_language: "Lua"
category:             "Lua"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/lua/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

# Convertendo strings para letras minúsculas em Lua

## O que e Por quê?

Converter uma string para letras minúsculas significa alterar todas as letras maiúsculas para as suas respectivas minúsculas. Os programadores fazem isso para manter a consistência dos dados, eliminar diferenças causadas por variações de caixa para facilitar as comparações de strings.

## Como Fazer:

Aqui está um exemplo simples de como converter uma string para letras minúsculas em Lua.

```Lua
string.upper = "ESTE É UM TESTE"
string.lower = string.upper:lower()
print(string.lower)
```

O resultado é:

```Lua
"este é um teste"
```
## Mergulho Profundo

1. **Contexto histórico:** Lua, uma linguagem de programação poderosa, eficiente e leve, desenvolveu-se com ênfase em funções de string desde o início. A funcionalidade de converter strings para minúsculas é parte integrante disso.

2. **Alternativas:** Embora a função `:lower()` seja a forma mais direta e comum de converter uma string para minúsculas em Lua, existem outras maneiras, como criar uma função personalizada usando a função `string.gsub()` juntamente com a tabela ASCII.

3. **Detalhes de implementação:** A função `:lower()` em Lua funciona percorrendo cada caractere da string, verifica se é uma letra maiúscula usando a tabela ASCII, e se for, converte para a respectiva letra minúscula. 

## Veja Também

Links para leituras relacionadas:
- Para entender melhor o tratamento de strings em Lua, veja: [Programming in Lua: Strings](https://www.lua.org/pil/20.html)
- Para uma exploração mais aprofundada das funções de string em Lua, leia: [Lua-Users: String Library Tutorial](http://lua-users.org/wiki/StringLibraryTutorial)