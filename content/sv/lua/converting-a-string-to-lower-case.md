---
title:                "Konvertera en sträng till gemener"
html_title:           "Lua: Konvertera en sträng till gemener"
simple_title:         "Konvertera en sträng till gemener"
programming_language: "Lua"
category:             "Lua"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/lua/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Att konvertera en sträng till gemener betyder att omvandla alla bokstäver i strängen till små bokstäver. Detta är en vanlig operation inom programmering och kan användas för att jämföra strängar utan att ta hänsyn till storleken på bokstäverna.

## Hur man:
```Lua
-- Ett exempel på hur man konverterar en sträng till gemener i Lua
str = "HELLO WORLD"
print(string.lower(str)) -- Outputs "hello world"
```

Vissa programmeringsspråk har inbyggda funktioner för att konvertera strängar till gemener, som i exemplet ovan med Lua. Om ditt språk inte har en sådan funktion finns det vanligtvis en annan lösning som involverar att loopa igenom strängen och konvertera varje bokstav separat.

## Djupdykning:
Konvertering av strängar till gemener har sina rötter i ASCII-kodningen från 1960-talet, där stora och små bokstäver kodades som olika tecken. Med tiden har det blivit ett standardiserat sätt att hantera strängar och idag är det en vanlig funktion i de flesta programmeringsspråk.

Det finns också alternativ till att använda en inbyggd funktion för att konvertera strängar till gemener. Till exempel kan du använda en reguljär uttryck för att matcha och ersätta alla stora bokstäver med sina små motsvarigheter. Detta kan vara användbart om du vill göra en mer komplex transformation på strängen.

Implementationen av konvertering från stora till små bokstäver beror på det specifika programmeringsspråket, men i allmänhet görs det genom att använda ASCII- eller Unicode-tabeller för att hitta motsvarande tecken.

## Se också:
För mer information om stränghantering och andra strängrelaterade funktioner i Lua, se Lua-dokumentationen: https://www.lua.org/pil/20.2.html