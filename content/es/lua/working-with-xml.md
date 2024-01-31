---
title:                "Trabajando con XML"
date:                  2024-01-26T04:33:32.705377-07:00
model:                 gpt-4-0125-preview
simple_title:         "Trabajando con XML"

category:             "Lua"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/lua/working-with-xml.md"
---

{{< edit_this_page >}}

## ¿Qué y Por Qué?
Trabajar con XML implica analizar y manipular documentos XML usando código. Los programadores hacen esto para leer, escribir y modificar datos en un formato estructurado y portátil que se usa ampliamente para el intercambio y almacenamiento de datos.

## Cómo hacerlo:
Lua no incluye análisis XML nativo, pero hay bibliotecas como LuaXML y xml2lua que hacen el trabajo. Aquí hay un vistazo rápido a cómo analizar XML con xml2lua:

```Lua
local xml2lua = require("xml2lua")
local handler = require("xmlhandler.tree")

local xmlParser = xml2lua.parser(handler)
xmlParser:parse([[<root><book id="123">Programación en Lua</book></root>]])

print(handler.root.book._attr.id)  -- Salidas: 123
print(handler.root.book[1])        -- Salidas: Programación en Lua
```

Para escribir XML, aquí hay un mini ejemplo usando LuaXML:

```Lua
local luaxml = require("LuaXML")

local xml = xml.new("root")
xml:append("book")[1] = "Programación en Lua"
xml.book._attr = {id="123"}

print(xml:tag())  -- Salidas: <root><book id="123">Programación en Lua</book></root>
```

## Profundización
XML, abreviatura de Extensible Markup Language, ha sido un estándar en la representación y el intercambio de datos desde mediados de los 90. Le da estructura a los datos y es legible tanto por humanos como por máquinas.

Aunque ahora se prefieren JSON y YAML por su simplicidad, XML sigue siendo prevalente en muchos sistemas empresariales y legados. En Lua, el manejo nativo de XML no está incorporado porque Lua está diseñado para ser pequeño y extensible a través de módulos.

Las bibliotecas de XML para Lua, como LuaXML, xml2lua y otras, cierran esta brecha. LuaXML proporciona un lector y escritor de XML ligero, mientras que xml2lua utiliza un enfoque basado en eventos similar a los analizadores SAX. Estas bibliotecas suelen implementarse en Lua puro para la portabilidad, aunque algunas pueden depender de C para el rendimiento.

Cuando se trata de rendimiento y uso de memoria, las bibliotecas XML de Lua pueden no ser tan rápidas como aquellas en lenguajes con soporte nativo. Sin embargo, para la mayoría de los casos de uso en Lua, especialmente en el desarrollo de juegos o scripts para sistemas empotrados, estas bibliotecas hacen un buen trabajo sin sobrecargar el sistema.

## Ver También
- LuaXML en GitHub: https://github.com/LuaDist/luaxml
- xml2lua en GitHub: https://github.com/manoelcampos/xml2lua
- Lista de bibliotecas de Lua.org: https://lua-users.org/wiki/LibrariesAndBindings
