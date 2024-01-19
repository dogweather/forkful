---
title:                "Excluindo caracteres que correspondem a um padrão"
html_title:           "Arduino: Excluindo caracteres que correspondem a um padrão"
simple_title:         "Excluindo caracteres que correspondem a um padrão"
programming_language: "Lua"
category:             "Lua"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/lua/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## O Que & Porque?

Excluir caracteres que coincidem com um padrão é uma operação em Lua que permite cleanzar strings de caracteres indesejados. Isso é extremamente útil quando se trabalha com dados impuros ou se deseja manipular strings de uma maneira específica.

## Como fazer:

Podemos usar as funções gsub e gmatch para excluir caracteres que coincidem com um padrão em Lua. Aqui está um exemplo:

```Lua
s = "lua é espantoso"
s = s:gsub("é espantoso", "")
print(s) -- saída: lua
```

No exemplo acima, ‘é espantoso’ é o padrão que queremos excluir. Como resultado, a saída é "lua".

## Análise mais profunda

Historicamente, o tratamento de strings em Lua sempre foi fortalecido através de suas poderosas bibliotecas de manipulação de padrões. Gsub e gmatch são funções agora comuns em Lua, mas existem outras ferramentas disponíveis em Lua como os métodos string.match, string.find, dentre outros que podem ser usados alternativamente, dependendo do problema em mãos.

Quando você chama string.gsub em Lua, o interpretador do Lua compara o padrão que você forneceu com a string original. Ele continua até que tenha percorrido toda a string ou até que não haja mais correspondências.

## Veja também

Para aprender mais sobre manipulação de string em Lua, você pode visitar os seguintes links:

1. Manual Lua 5.4: https://www.lua.org/manual/5.4/manual.html#6.4 
2. Tutorial Lua: https://www.tutorialspoint.com/lua/lua_strings.htm 
3. Guia de manipulação de strings de Lua: https://riptutorial.com/lua/topic/1345/string-manipulation