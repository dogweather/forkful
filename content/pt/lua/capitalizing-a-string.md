---
title:                "Capitalizando uma string"
html_title:           "Lua: Capitalizando uma string"
simple_title:         "Capitalizando uma string"
programming_language: "Lua"
category:             "Lua"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/lua/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## O Que & Porquê?

Em programação, capitalizar uma cadeia significa transformar o primeiro caractere de cada termo em sua versão maiúscula. Isso é comum para melhorar a legibilidade e garantir a padronização dos dados.

## Como Fazer:

Em Lua, podemos capitalizar uma string utilizando a função `gsub` combinada com `upper`:

```Lua
texto = "Olá, mundo!"
texto_capitalizado = (texto:gsub("^%l", string.upper))
print(texto_capitalizado)  -- Saída: "Olá, Mundo!"
```

A primeira letra de cada palavra pode ser capitalizada com:

```Lua
texto = "olá, mundo lua!"
texto_capitalizado = (texto:gsub("(%a)([%w_']*)", function(first, rest) return first:upper()..rest:lower() end))
print(texto_capitalizado)  -- Saída: "Olá, Mundo Lua!"
```

## Análise Mais Profunda:

A capitalização em Lua é feita através da transformação direta dos códigos ASCII do texto, um a um. A maiúscula de cada letra é obtida subtraindo 32 do código ASCII correspondente da minúscula.

Quanto a alternativas, outras linguagens de programação possuem funções específicas para capitalização. Em Lua, precisamos fazer uso das funções `gsub` e `upper`.

É importante mencionar que a capitalização em Lua é case-sensitive, ou seja, faz distinção entre letras maiúsculas e minúsculas. Portanto, é importante cuidado ao usar funções relacionadas a capitalização. 

## Ver Também:

Para aprender mais sobre strings em Lua:
- A documentação oficial sobre Strings: https://www.lua.org/manual/5.4/manual.html#6.4
- Wiki sobre funções de string em Lua: http://lua-users.org/wiki/StringLibraryTutorial
- Stack Overflow, para perguntas e respostas relacionadas a Lua: https://stackoverflow.com/questions/tagged/lua