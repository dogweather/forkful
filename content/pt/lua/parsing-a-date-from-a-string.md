---
title:                "Analisando uma data de uma string"
html_title:           "Lua: Analisando uma data de uma string"
simple_title:         "Analisando uma data de uma string"
programming_language: "Lua"
category:             "Lua"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/lua/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## O que & Por quê?

Parsear uma data de uma string é o processo de converter uma data presente em uma string para um formato que possa ser utilizado e manipulado em um programa de computador. Isso é útil para programadores que precisam trabalhar com datas em seus códigos, e também para usuários finais que desejam inserir datas de forma mais fácil e legível.

## Como fazer:

```Lua
-- Exemplo de como parsear uma data de formato comum (DD/MM/YYYY) em Lua.
local data = "01/04/2021" -- string contendo a data a ser parseada
local dia, mes, ano = data:match("(%d+)/(%d+)/(%d+)") -- utilizando o método match para extrair o dia, mês e ano da string
-- note que os valores extraídos estarão em formato de string, então é necessário convertê-los para números caso necessário
print("O dia é: " .. dia) -- saída: O dia é: 01
print("O mês é: " .. mes) -- saída: O mês é: 04
print("O ano é: " .. ano) -- saída: O ano é: 2021
```

## Aprofundando:

Para entender melhor como o processo de parsear datas de uma string funciona, é importante conhecer o histórico por trás dessa técnica. Antigamente, datas eram armazenadas em formatos específicos de acordo com a máquina e o sistema operacional, o que tornava difícil utilizar essas informações em diferentes plataformas. Com o avanço da tecnologia e a padronização de datas em formatos universais, o parsing se tornou uma ferramenta essencial para lidar com datas em programas de computador.

Existem diversas formas de fazer o parsing de datas em Lua, incluindo a utilização de expressões regulares e módulos específicos para essa tarefa. No entanto, o método mostrado acima é uma das formas mais comuns e simples de se fazer o parsing em Lua.

## Veja também:

- [Lua.org](https://www.lua.org/): Página oficial da linguagem de programação Lua.
- [Lua-Users.org](http://lua-users.org/wiki/Main_Page): Comunidade de usuários de Lua, com fóruns e artigos relacionados à linguagem.
- [Tutorial de Lua](https://www.tutorialspoint.com/lua/index.htm): Tutorial completo e bem explicado da linguagem Lua.