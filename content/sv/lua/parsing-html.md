---
title:                "Tolka HTML"
date:                  2024-01-20T15:32:45.998359-07:00
simple_title:         "Tolka HTML"

tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/lua/parsing-html.md"
---

{{< edit_this_page >}}

## Vad & Varför?

Parsing HTML betyder att du läser och tolkar HTML-kod så att du kan manipulera eller extrahera specifik data från den. Programmerare parser HTML för att automatisera webbskrapning, databearbetning, eller för att integrera externa webbinnehåll i sina applikationer.

## Hur gör man:

För att parse HTML i Lua, kan vi använda `lua-html` biblioteket. Här är ett grundläggande exempel:

```lua
local html = require("html")

local code = [[
<!DOCTYPE html>
<html>
<head>
    <title>Test Page</title>
</head>
<body>
    <h1>Welcome to my website</h1>
    <p>This is a test paragraph.</p>
</body>
</html>
]]

local parsed = html.parse(code)
for _, element in ipairs(parsed:select("p")) do
    print(element:getcontent())
end
```

Sample output:
```
This is a test paragraph.
```

## Djupdykning

HTML-parsing har funnits ända sedan webben blev populär. Tidiga verktyg var ofta skräddarsydda och bräckliga. Numera finns det robusta bibliotek som `lua-html`. Alternativa bibliotek som `luasoup` eller inbyggda funktioner i web frameworks kan också användas. När du parser HTML är det viktigt att hantera illa formaterad HTML, vilket är vanligt. `lua-html` hanterar detta fint genom sin förmåga att korrigera och tolka oregelbunden HTML.

## Se även:

För mer om Lua och HTML-parsing, kolla in följande resurser:
