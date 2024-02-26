---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:12:39.391548-07:00
description: "Att tolka HTML inneb\xE4r att extrahera data och information fr\xE5\
  n HTML-dokument, vilket \xE4r avg\xF6rande f\xF6r webskrapning, dataanalys och\u2026"
lastmod: '2024-02-25T18:49:36.339907-07:00'
model: gpt-4-0125-preview
summary: "Att tolka HTML inneb\xE4r att extrahera data och information fr\xE5n HTML-dokument,\
  \ vilket \xE4r avg\xF6rande f\xF6r webskrapning, dataanalys och\u2026"
title: Tolka HTML
---

{{< edit_this_page >}}

## Vad & Varför?
Att tolka HTML innebär att extrahera data och information från HTML-dokument, vilket är avgörande för webskrapning, dataanalys och automatiseringsuppgifter. Programmerare utför detta för att samla, analysera eller manipulera webbinnehåll programmatiskt, vilket möjliggör automatisering av vad som annars skulle vara manuell extraktion av data från webbplatser.

## Hur man gör:
Lua har inte ett inbyggt bibliotek för att tolka HTML, men du kan använda dig av tredjepartsbibliotek som `LuaHTML` eller utnyttja bindningar för `libxml2` genom `LuaXML`. En populär metod är att använda biblioteket `lua-gumbo` för att tolka HTML, vilket ger en enkel, HTML5-kompatibel tolkningsförmåga.

### Installera lua-gumbo:
Först, se till att `lua-gumbo` är installerat. Du kan vanligtvis installera det med luarocks:

```sh
luarocks install lua-gumbo
```

### Grundläggande tolkning med lua-gumbo:
Så här kan du tolka en enkel HTML-snutt och extrahera data från den med `lua-gumbo`:

```lua
local gumbo = require "gumbo"
local dokument = gumbo.parse[[<html><body><p>Hej, världen!</p></body></html>]]

local p = dokument:getElementsByTagName("p")[1]
print(p.textContent)  -- Utmatning: Hej, världen!
```

### Avancerat exempel - Extrahera länkar:
För att extrahera `href`-attribut från alla ankartaggar (`<a>`-element) i ett HTML-dokument:

```lua
local gumbo = require "gumbo"
local dokument = gumbo.parse([[
<html>
<head><title>Exempelsida</title></head>
<body>
  <a href="http://example.com/1">Länk 1</a>
  <a href="http://example.com/2">Länk 2</a>
  <a href="http://example.com/3">Länk 3</a>
</body>
</html>
]])

for _, element in ipairs(dokument.links) do
    if element.getAttribute then  -- Försäkra att det är ett Element och har attribut
        local href = element:getAttribute("href")
        if href then print(href) end
    end
end

-- Exempel på utmatning:
-- http://example.com/1
-- http://example.com/2
-- http://example.com/3
```

Denna kodsnutt itererar igenom alla länkar i dokumentet och skriver ut deras `href`-attribut. `lua-gumbo`-bibliotekets förmåga att tolka och förstå strukturen på ett HTML-dokument förenklar processen att extrahera specifika element baserat på deras taggar eller attribut.
