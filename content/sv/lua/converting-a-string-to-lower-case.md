---
title:                "Konvertera en sträng till gemener"
aliases:
- sv/lua/converting-a-string-to-lower-case.md
date:                  2024-01-20T17:39:08.966988-07:00
model:                 gpt-4-1106-preview
simple_title:         "Konvertera en sträng till gemener"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/lua/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Att konvertera en sträng till små bokstäver innebär att omvandla alla tecken i strängen till deras motsvarighet i gemener. Programmerare gör detta för att förenkla jämförelser och sökningar, eftersom det tar bort skillnader mellan versaler och gemener.

## Hur man gör:
För att konvertera en sträng till små bokstäver i Lua, använd den inbyggda funktionen `string.lower()`. Här är ett exempel:

```Lua
local originalString = "Hej Världen!"
local lowerCaseString = string.lower(originalString)
print(lowerCaseString)  -- Output: "hej världen!"
```

Och ett till, med svenska tecken:

```Lua
local swedishString = "Älg i ÅÄÖ"
local lowerCaseSwedish = string.lower(swedishString)
print(lowerCaseSwedish) -- Output: "älg i åäö"
```

Kom ihåg, `string.lower()` funkar bra med svenska karaktärer också.

## Djupdykning
Funktionen `string.lower()` i Lua har varit en del av standardbiblioteket sedan tidiga versioner av språket. Den använder interna teckentabeller för att mappa versaler till gemener. Detta är typiskt för de flesta programmeringsspråk. Alternativ till `string.lower()` kan inkludera egenimplementerade funktioner för specialfall där olika språkliga regler gäller. I Lua, arbetar `string.lower()` tillförlitligt för både ASCII och UTF-8-kodade tecken, vilket gör den tillräckligt robust för användning med de flesta skriftsystem, inklusive det svenska alfabetet.

Det är även värt att nämna att prestanda för string-manipulation kan variera beroende på längden av strängen och vilken underliggande implementation av Lua tolken du använder.

## Se också
- Lua's officiella dokumentation för string-biblioteket: https://www.lua.org/manual/5.4/manual.html#6.4
- För en mer omfattande förståelse om Unicode och teckenkodningar: https://unicode.org/
- För att experimentera med Lua-kod direkt i webbläsaren: https://www.lua.org/demo.html

Notera att externa länkar är på engelska. Detta material kan hjälpa till att ytterligare utforska string-manipulation och relaterade ämnen.
