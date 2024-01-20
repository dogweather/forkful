---
title:                "Att skapa en tillfällig fil"
html_title:           "Bash: Att skapa en tillfällig fil"
simple_title:         "Att skapa en tillfällig fil"
programming_language: "Lua"
category:             "Lua"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/lua/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Vad och varför?
Att skapa en temporär fil innebär att vi skapar en fil som systemet tar bort efter att den inte används längre. Programmerare gör detta för att spara tillfällig data som inte behöver lagras permanent och för att reducera minnesanvändning.

## Hur till:
Här är hur du skapar och använder en temporär fil i Lua:

```Lua
os.execute("touch tmp.txt")

-- Skriv till temporary filen
local file = io.open("tmp.txt", "w")
file:write("Hej Världen!\n")
file:close()

-- Läs från temporary filen
local file = io.open("tmp.txt", "r")
io.input(file)
print(io.read())
file:close()

os.execute("rm tmp.txt")
```

När du kör detta program kommer du att se följande utskrift:

```
Hej Världen!
```

## Djupare Dykning
Historiskt sett har temporära filer använts sedan begynnelsen av datortiden, men det var inte förrän med moderna högnivåspråk som Lua att de blev enkla att implementera och använda.

Alternativ till att skapa temporära filer inkluderar användning av databaser eller minneslagringslösningar som MongoDB eller Redis, men dessa kan vara överkomplicerade för mindre applikationer.

Lua hanterar temporära filer på operativsystemnivå, vilket innebär att din Laravel-applikation måste ha tillräckliga rättigheter för att skapa och radera filer på systemet. Generellt sett ansvarar Lua för rensning och borttagning av dessa filer, men det är alltid bra praxis att manuellt ta bort temporära filer när du är klar med dem för att undvika onödigt skräp på disken.

## Se även:
* [Lua Documentation: File Objects](https://www.lua.org/pil/21.2.2.html)
* [OS Library Functions in Lua](https://www.lua.org/pil/22.html)