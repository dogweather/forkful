---
title:                "Läsning av en textfil"
html_title:           "Lua: Läsning av en textfil"
simple_title:         "Läsning av en textfil"
programming_language: "Lua"
category:             "Lua"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/lua/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Vad & Varför?

Att läsa en textfil innebär att man tar in information från en extern fil och använder den i sitt program. Programmare gör detta för att kunna läsa och använda externa data, till exempel konfigurationsfiler eller användarinmatning.

## Hur man gör:

```
Lua:
-- Öppna filen för läsning
local file = io.open("textfil.txt", "r")

-- Läs innehållet av filen och lägg det i en variabel
local innehåll = file:read("*all")

-- Stäng filen
file:close()

-- Skriv ut innehållet
print(innehåll)

```

## Djupdykning:

Att läsa textfiler har funnits sedan början av programmering, och är en av de mest grundläggande sätten att interagera med externa data. Alternativ till att läsa textfiler kan vara att använda en databas eller att lagra data direkt i programmet. För att läsa en textfil i Lua används io biblioteket, som har funktioner som möjliggör läsning, skrivning och stängning av filer.

## Se även:

- [Lua Reference Manual](https://www.lua.org/manual/5.4/)
- [io Library Documentation](https://www.lua.org/manual/5.4/manual.html#6.8)