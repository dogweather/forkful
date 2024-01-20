---
title:                "Analisando HTML"
html_title:           "Arduino: Analisando HTML"
simple_title:         "Analisando HTML"
programming_language: "Lua"
category:             "Lua"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/lua/parsing-html.md"
---

{{< edit_this_page >}}

## O Que & Por Que?

Analisar HTML significa extrair informações de um documento HTML. Programadores fazem isso para manipular, extrair dados, ou interagir com sites baseados em HTML de maneira programática.

## Como Fazer:
Você pode usar uma biblioteca chamada LuaHtmlParser. Aqui está um exemplo básico:

```Lua
local luahtmlparser = require("luahtmlparser")

local parser = luahtmlparser.new()
parser:add_text("<html><body><p>Olá, mundo!</p></body></html>")

local root = parser:parse()
print(root:select("p"):text())
```
Saída:

```
Olá, mundo!
```

## Mergulho Profundo
Lua, criado em 1993, é conhecido por ser leve e eficiente. A história do parsing de HTML na Lua tem sido uma história de opções de terceiros, como o LuaExpat ou a LuaHtmlParser que usamos acima.

Alternativas para parsear HTML incluem o uso de expressões regulares, embora seja geralmente desaconselhado devido à irregularidade do HTML na prática. Para HTML bem formado, algoritmos de análise com base em pilha também podem ser usados.

Em termos de implementação, o parsing de HTML envolve uma análise em duas etapas: a tokenização para transformar o texto em uma sequência de tokens (tags, atributos, etc.), e a construção desta sequência de tokens em uma estrutura de árvore, conhecida como "Árvore do Objeto do Documento" (DOM).

## Veja Também
- [Documentação LuaHtmlParser](https://github.com/msva/lua-htmlparser)
- [Por que não usar regex para analisar HTML](https://stackoverflow.com/questions/1732348/regex-match-open-tags-except-xhtml-self-contained-tags)
- [Lua User Wiki: Parsing HTML](https://lua-users.org/wiki/ScrapingHtml)
- [Manual de Referência Lua 5.4](https://www.lua.org/manual/5.4/)