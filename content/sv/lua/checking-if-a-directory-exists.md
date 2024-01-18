---
title:                "Att kontrollera om en mapp existerar"
html_title:           "Lua: Att kontrollera om en mapp existerar"
simple_title:         "Att kontrollera om en mapp existerar"
programming_language: "Lua"
category:             "Lua"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/lua/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Att kontrollera om en mapp existerar i Lua handlar om att få reda på om en specifik mapp finns på ditt system eller inte. Detta är viktigt för programmerare eftersom det gör det möjligt att hantera filer och resurser på ett effektivt sätt, och att undvika felmeddelanden.

## Hur man gör:
Kontrollera om en mapp existerar i Lua är enkelt. Du behöver bara använda funktionen "lfs.attributes" och ange sökvägen till mappen du vill kontrollera. Om funktionen returnerar en tabell, betyder det att mappen finns. Annars finns den inte. Här är ett exempel på kod:

```Lua
if lfs.attributes("mappnamn", "mode") then
  print("Mappen finns!")
else
  print("Mappen finns inte!")
end
```
Om mappen finns kommer "Mappen finns!" att skrivas ut, annars kommer "Mappen finns inte!" att skrivas ut.

## Djupdykning:
Att kontrollera om en mapp existerar i Lua är en vanlig uppgift för programmerare. Det används ofta för att undvika felmeddelanden när man försöker nå en mapp som inte finns eller för att hantera olika scenarier beroende på om mappen finns eller inte.

En annan metod för att kontrollera om en mapp existerar är att använda "os.execute" funktionen och köra kommandot "dir" (för Windows) eller "ls" (för Unix-system) för att lista innehållet i en mapp. Om mappen finns kommer det att visas i listan, annars inte.

Det är också värt att notera att "lfs.attributes" funktionen kan användas för att få annan information om en mapp, såsom datumet den senast ändrades eller dess storlek.

## Se även:
För mer information om "lfs.attributes" och andra funktioner i LFS (Lua File System) biblioteket, besök https://keplerproject.github.io/luafilesystem/. Du kan också läsa mer om hantering av filer och mappar i Lua på https://www.lua.org/pil/21.html.