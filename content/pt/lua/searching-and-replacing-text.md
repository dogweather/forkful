---
title:                "Pesquisando e substituindo texto"
html_title:           "Bash: Pesquisando e substituindo texto"
simple_title:         "Pesquisando e substituindo texto"
programming_language: "Lua"
category:             "Lua"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/lua/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

# Substituindo Textos na Lua: Guia Prático e Aprofundado

## O Que & Por Quê?

Substituição de texto é o processo de busca e alteração de uma sequência específica em um bloco de texto. Programadores utilizam isso frequentemente para manipulação de strings e alteração de dados em textos.

## Como Fazer:

Utilizamos a função `gsub` em Lua para substituir texto, aqui está um exemplo:
```Lua
s = "Olá, Mundo!"
s = string.gsub(s, "Mundo", "Lua")
print(s)
```
Na execução, tal código resultará em:
```
Olá, Lua!
```
No código acima, a string "Mundo" é substituída por "Lua".

## Mergulho Profundo

### Contexto Histórico

Lua, do português "Lua", é uma linguagem de script de alto nível criada em 1993 por um grupo brasileiro. Lua não foi originalmente projetada para manipulação de texto, mas essa funcionalidade foi adicionada posteriormente como parte de suas bibliotecas padrão.

### Alternativas

Existem outras funções em Lua que também podem buscar e substituir texto, como `string.find` e `string.match`. Cada função tem seus próprios usos dependendo dos requisitos do usuário.

### Detalhes da Implementação

A função `gsub` usa expressões regulares para encontrar e substituir texto. Ela varre a string de entrada e, para cada correspondência da sequência de busca, substitui-a pela sequência de substituição.

## Veja Também

Para mais informações sobre programação com Lua, confira estes links úteis:
* [Site Oficial da Lua](http://www.lua.org/)
* [Documentação da Lua 5.4](http://www.lua.org/manual/5.4/)
* [Guia de Programação em Lua](https://www.lua.org/pil/)

Terminamos nosso tour pela substituição de textos em Lua. Você está agora pronto para começar a manipular strings como um pro!