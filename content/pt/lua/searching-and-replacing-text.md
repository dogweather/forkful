---
title:                "Buscando e substituindo texto"
html_title:           "Lua: Buscando e substituindo texto"
simple_title:         "Buscando e substituindo texto"
programming_language: "Lua"
category:             "Lua"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/lua/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## O Que & Porquê?
Substituir texto é uma técnica utilizada por programadores para manipular e editar grandes quantidades de dados em seus códigos. É especialmente útil para fazer alterações em vários trechos de texto de uma só vez, economizando tempo e esforço. Muitas vezes, é necessário substituir texto para corrigir erros de digitação, atualizar informações ou modificar cadeias de caracteres em vários locais no código.

## Como Fazer:
Para substituir texto em Lua, podemos utilizar a função `string.gsub()` que recebe três argumentos: a string que será modificada, o texto que será buscado e o texto que será inserido no lugar do encontrado. Por exemplo:

```
Lua
local texto = "Olá, Mundo!"
local novo_texto = string.gsub(texto, "Mundo", "Lua")
print(novo_texto)
-- Saída: Olá, Lua!
```

Também é possível utilizar expressões regulares para buscar e substituir texto. A sintaxe é a seguinte: ```string.gsub(texto, "padrão", função_de_substituição)```. Por exemplo:

```
lua
local texto = "Meu nome é João. Estou no nível 10."
local novo_texto = string.gsub(texto, "(%a+)%.(%a+)", "%2, %1")
print(novo_texto)
--Saída: Meu sobrenome é João, nome é nível 10.
```

## Mergulho Profundo:
A prática de substituir texto remonta aos primórdios da programação e continua sendo uma ferramenta fundamental nos dias de hoje. Existem alternativas para substituir texto, como a utilização de laços de repetição, porém a função `string.gsub()` é mais rápida e eficiente. Além disso, é possível realizar buscas e substituições em diferentes formas de armazenamento de texto, como arquivos e bancos de dados.

## Veja Também:
Para saber mais sobre a função `string.gsub()` e expressões regulares em Lua, confira a documentação oficial em https://www.lua.org/manual/5.3/manual.html#pdf-string.gsub e o tutorial de expressões regulares em https://www.lua.org/pil/20.2.html. Além disso, o site Regex101 oferece uma ferramenta online para testar e construir suas próprias expressões regulares em Lua.