---
title:                "Interpolando uma string."
html_title:           "Lua: Interpolando uma string."
simple_title:         "Interpolando uma string."
programming_language: "Lua"
category:             "Lua"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/lua/interpolating-a-string.md"
---

{{< edit_this_page >}}

## O que é e por quê?

Interpolar uma string é uma técnica de programação em que você combina strings (texto) e variáveis ​​para criar uma nova string. Isso é útil para criar mensagens e informações dinâmicas em um programa. Os programadores usam a interpolação de strings para tornar seus códigos mais dinâmicos e flexíveis.

## Como fazer:

```
-- Exemplo 1:
nome = "João"
idade = 25
print("Olá, meu nome é " .. nome .. " e eu tenho " .. idade .. " anos.")

-- Saída: Olá, meu nome é João e eu tenho 25 anos.


-- Exemplo 2:
animal = "gato"
print("Eu tenho um " .. animal .. " e ele se chama " .. nome)

-- Saída: Eu tenho um gato e ele se chama João.
```

## Exploração profunda:

A interpolação de strings já existe há bastante tempo na programação, mas se tornou popular com o surgimento de linguagens de script como Lua. Os programadores geralmente usam a concatenação de strings (juntar strings usando o operador "..") como uma alternativa à interpolação de strings. No entanto, a interpolação pode ser mais simples e legível em casos em que muitas variáveis ​​precisam ser combinadas em uma string.

Uma técnica comum para a interpolação de strings é usar a função `string.format()`, que permite formatar strings com valores de variáveis ​​inseridos em locais específicos. Além disso, muitos frameworks e bibliotecas de Lua, como o LÖVE e o Torch, fornecem recursos de interpolação de strings incorporados para facilitar o uso.

## Veja também:

- [Documentação oficial do Lua sobre interpolação de strings](https://www.lua.org/pil/20.2.html)
- [Tutorial sobre interpolação de strings em Lua](https://learnxinyminutes.com/docs/lua/)
- [Exemplos de uso de interpolação de strings em diferentes situações](https://www.tutorialspoint.com/lua/lua_string_interpolation.htm)