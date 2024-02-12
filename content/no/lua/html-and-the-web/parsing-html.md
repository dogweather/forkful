---
title:                "Analysering av HTML"
aliases:
- no/lua/parsing-html.md
date:                  2024-02-03T19:12:43.590611-07:00
model:                 gpt-4-0125-preview
simple_title:         "Analysering av HTML"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/lua/parsing-html.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Hva & hvorfor?
Parsing av HTML innebærer å trekke ut data og informasjon fra HTML-dokumenter, noe som er avgjørende for webskraping, dataanalyse og automatiseringsoppgaver. Programutviklere utfører dette for å samle, analysere eller manipulere webinnhold programmatisk, noe som muliggjør automatisering av det som ellers ville vært manuell uttrekking av data fra nettsider.

## Hvordan:
Lua har ikke et innebygd bibliotek for parsing av HTML, men du kan utnytte tredjepartsbiblioteker som `LuaHTML` eller bruke bindinger for `libxml2` gjennom `LuaXML`. En populær tilnærming er å bruke biblioteket `lua-gumbo` for parsing av HTML, som gir en enkel, HTML5-kompatibel parsingsevne.

### Installere lua-gumbo:
Først, sørg for at `lua-gumbo` er installert. Du kan vanligvis installere det ved hjelp av luarocks:

```sh
luarocks install lua-gumbo
```

### Grunnleggende parsing med lua-gumbo:
Her er hvordan du kan parse et enkelt HTML-snutt og trekke ut data fra det ved bruk av `lua-gumbo`:

```lua
local gumbo = require "gumbo"
local document = gumbo.parse[[<html><body><p>Hei, verden!</p></body></html>]]

local p = document:getElementsByTagName("p")[1]
print(p.textContent)  -- Utdata: Hei, verden!
```

### Avansert eksempel - Utvinning av lenker:
For å trekke ut `href`-attributter fra alle ankertagger (`<a>`-elementer) i et HTML-dokument:

```lua
local gumbo = require "gumbo"
local document = gumbo.parse([[
<html>
<head><title>Eksempelside</title></head>
<body>
  <a href="http://example.com/1">Lenke 1</a>
  <a href="http://example.com/2">Lenke 2</a>
  <a href="http://example.com/3">Lenke 3</a>
</body>
</html>
]])

for _, element in ipairs(document.links) do
    if element.getAttribute then  -- Sørg for at det er et Element og har attributter
        local href = element:getAttribute("href")
        if href then print(href) end
    end
end

-- Eksempel på utdata:
-- http://example.com/1
-- http://example.com/2
-- http://example.com/3
```

Dette kodeutdraget itererer gjennom alle lenkene i dokumentet og skriver ut deres `href`-attributter. `lua-gumbo`-bibliotekets evne til å parse og forstå strukturen av et HTML-dokument forenkler prosessen med å trekke ut spesifikke elementer basert på deres tagger eller attributter.
