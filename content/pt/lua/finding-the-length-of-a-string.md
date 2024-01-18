---
title:                "Encontrando o comprimento de uma string"
html_title:           "Lua: Encontrando o comprimento de uma string"
simple_title:         "Encontrando o comprimento de uma string"
programming_language: "Lua"
category:             "Lua"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/lua/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## O que e Por que?
Encontrar o comprimento de uma string é uma tarefa comum para programadores em Lua. Essa ação envolve determinar a quantidade de caracteres em uma determinada string e é útil para diversas tarefas, como validar inputs de usuário, formatar saídas de texto e realizar operações com strings.

## Como fazer:
Para encontrar o comprimento de uma string em Lua, podemos utilizar a função `string.len()`. Veja o exemplo abaixo:
```Lua
nome = "João"
print(string.len(nome)) -- Output: 4
```

Você também pode utilizar essa função em uma string contida em uma variável, veja:
```Lua
texto = "Olá, tudo bem?"
print(string.len(texto)) -- Output: 15
```

## Profundando:
Ao contrário de outras linguagens de programação, como C ou Java, Lua não possui uma função interna para encontrar o comprimento de uma string. Porém, a função `string.len()` possui uma implementação eficiente e pode ser utilizada em qualquer tipo de string, incluindo caracteres especiais e unicode.

Uma alternativa para encontrar o comprimento de uma string em Lua é utilizar um loop para percorrer cada caractere da string e incrementar uma variável contador. Porém, essa abordagem é menos eficiente e mais trabalhosa.

## Veja Também:
- Documentação oficial do Lua sobre a função string.len(): https://www.lua.org/manual/5.3/manual.html#6.4.1
- Função string.len() em ação no Lua Online Compiler: https://www.lua.org/demo.html#6.4.1
- Exemplos práticos de uso da função string.len(): https://www.tutorialspoint.com/lua/lua_strings.htm