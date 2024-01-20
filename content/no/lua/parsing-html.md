---
title:                "Analyse av HTML"
date:                  2024-01-20T15:33:12.687950-07:00
html_title:           "Arduino: Analyse av HTML"
simple_title:         "Analyse av HTML"
programming_language: "Lua"
category:             "Lua"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/lua/parsing-html.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Parsing av HTML er å tolke og omforme HTML-koden til en struktur programmer kan jobbe med. Vi gjør dette for å hente ut eller manipulere data fra nettsider.

## Slik gjør du:
```Lua
local htmlparser = require("htmlparser")

local html = [[
<!DOCTYPE html>
<html>
<head>
    <title>Eksempelside</title>
</head>
<body>
    <h1>Hei, Norge!</h1>
    <p>Dette er en eksempeltekst.</p>
</body>
</html>
]]

local root = htmlparser.parse(html)
local headings = root:select("h1")

for _, heading in ipairs(headings) do
    print(heading:getcontent())  -- Outputter teksten innenfor <h1>-taggen
end
```
Output:
```
Hei, Norge!
```

## Dypdykk
Parsing av HTML startet for å kunne hente ut og bearbeide informasjon fra nettsider. Rundt midten av 90-tallet, med økningen av internettet, ble behovet større. Alternativer til Lua for HTML-parsing inkluderer biblioteker i språk som Python (BeautifulSoup) og JavaScript (Cheerio). Ved implementasjon er det viktig med robust håndtering fordi HTML ofte er ustrukturert og inneholder mangler.

## Se Også
- Lua HTML-parser repo på GitHub: [https://github.com/msva/lua-htmlparser](https://github.com/msva/lua-htmlparser)
- Lua-dokumentasjon: [https://www.lua.org/manual/5.4/](https://www.lua.org/manual/5.4/)
- W3Schools HTML Tutorial: [https://www.w3schools.com/html/](https://www.w3schools.com/html/)