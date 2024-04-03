---
date: 2024-01-26 04:33:27.267805-07:00
description: "Como: Lua n\xE3o inclui an\xE1lise de XML nativa, mas existem bibliotecas\
  \ como LuaXML e xml2lua que realizam o trabalho. Aqui est\xE1 uma r\xE1pida olhada\
  \ na an\xE1lise\u2026"
lastmod: '2024-03-13T22:44:46.733243-06:00'
model: gpt-4-0125-preview
summary: "Lua n\xE3o inclui an\xE1lise de XML nativa, mas existem bibliotecas como\
  \ LuaXML e xml2lua que realizam o trabalho."
title: Trabalhando com XML
weight: 40
---

## Como:
Lua não inclui análise de XML nativa, mas existem bibliotecas como LuaXML e xml2lua que realizam o trabalho. Aqui está uma rápida olhada na análise de XML com xml2lua:

```Lua
local xml2lua = require("xml2lua")
local handler = require("xmlhandler.tree")

local xmlParser = xml2lua.parser(handler)
xmlParser:parse([[<root><book id="123">Programação em Lua</book></root>]])

print(handler.root.book._attr.id)  -- Saída: 123
print(handler.root.book[1])        -- Saída: Programação em Lua
```

Para escrever XML, aqui está um mini-exemplo usando LuaXML:

```Lua
local luaxml = require("LuaXML")

local xml = xml.new("root")
xml:append("book")[1] = "Programação em Lua"
xml.book._attr = {id="123"}

print(xml:tag())  -- Saída: <root><book id="123">Programação em Lua</book></root>
```

## Mergulho Profundo
XML, abreviação de Extensible Markup Language, tem sido um padrão em representação e troca de dados desde meados dos anos 90. Ele dá estrutura aos dados e é tanto legível por humanos quanto interpretável por máquinas.

Embora JSON e YAML agora sejam preferidos por sua simplicidade, o XML permanece prevalente em muitos sistemas empresariais e legados. Em Lua, o manuseio nativo do XML não é integrado porque Lua é projetada para ser pequena e extensível por meio de módulos.

Bibliotecas XML para Lua, como LuaXML, xml2lua e outras, preenchem essa lacuna. LuaXML fornece um leitor e escritor de XML leve, enquanto xml2lua usa uma abordagem baseada em eventos semelhante aos analisadores SAX. Essas bibliotecas geralmente são implementadas em Lua pura para portabilidade, enquanto algumas podem depender de C para desempenho.

Quando se trata de desempenho e uso de memória, as bibliotecas XML de Lua podem não ser tão rápidas quanto aquelas em linguagens com suporte nativo. No entanto, para a maioria dos casos de uso em Lua, especialmente no desenvolvimento de jogos ou scripts para sistemas embarcados, essas bibliotecas fazem um bom trabalho sem sobrecarregar o sistema.

## Veja Também
- LuaXML no GitHub: https://github.com/LuaDist/luaxml
- xml2lua no GitHub: https://github.com/manoelcampos/xml2lua
- Lista de bibliotecas do Lua.org: https://lua-users.org/wiki/LibrariesAndBindings
