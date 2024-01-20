---
title:                "Analysering av html"
html_title:           "C#: Analysering av html"
simple_title:         "Analysering av html"
programming_language: "Lua"
category:             "Lua"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/lua/parsing-html.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?

Å parse HTML er prosessen med å analysere HTML-koden til en nettside for å forstå dens struktur. Programmerere gjør dette for å hente, manipulere, eller endre spesifikk data fra websider.

## Slik gjør du det:

Lua gir ikke innebygd støtte til HTML parsing. Vi bruker et tredjeparts bibliotek som "htmlparser". Installere det med luarocks:

```Lua
luarocks install htmlparser
```

Her er et enkelt eksempel på hvordan du kan bruke det:

```Lua
local htmlparser = require "htmlparser"

local html = [[<body>Hei, verden!</body>]]
local root = htmlparser.parse(html)
local body = root("body"):get(1)

print(body:getcontent())  -- Outputs: Hei, verden!
```

## Dyp Dykk

HTML-parsing er ikke en ny idé. Internett har revolusjonert hele verden, og det var behovet for å analysere og manipulere nettinnhold. Alternativene til "htmlparser" inkluderer "htmlua", som gir mer fleksibilitet, men kan være mer komplisert for nybegynnere.

Ved parsing, "htmlparser" konverterer HTML-teksten til et sett med noder, noe som gjør det lettere å navigere i HTML. Det er basert på "tagsoup" -paradigmet, som er i stand til å håndtere ugyldig og dårlig formatert HTML, noe som er et vanlig scenario i det virkelige liv.

## Se Også

For mer komplekse behov, kan du ta en titt på "htmlua" (https://github.com/msva/lua-htmlparser). Men hvis du er ny til konseptet, vil "html parser" være det beste alternativet (https://github.com/msva/lua-htmlparser).