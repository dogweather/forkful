---
title:                "Controleren of een directory bestaat"
aliases:
- /nl/lua/checking-if-a-directory-exists/
date:                  2024-01-28T21:56:25.856269-07:00
model:                 gpt-4-0125-preview
simple_title:         "Controleren of een directory bestaat"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/nl/lua/checking-if-a-directory-exists.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Wat & Waarom?

Controleren of een map bestaat betekent verifiëren of een folder aanwezig is in het bestandssysteem. Programmeurs doen dit om fouten te voorkomen, zoals proberen te lezen van of schrijven naar een niet-bestaande locatie, wat een programma kan laten crashen of gegevens kan corrumperen.

## Hoe te:

Lua heeft geen ingebouwde mapafhandeling in zijn standaardbibliotheken. Je gebruikt vaak `os.execute` met `test` op Unix of `os.getenv` op Windows. Hier is hoe je het doet:

```lua
local function is_directory_exists(path)
    if package.config:sub(1,1) == '\\' then -- controleer voor Windows
        local cd_result = os.execute('cd ' .. path .. ' 2>nul')
        return cd_result == true or cd_result == 0
    else -- neem aan Unix-achtig
        local test_result = os.execute('[ -d "' .. path .. '" ]')
        return test_result == true or test_result == 0
    end
end

print(is_directory_exists("/pad/om/te/controleren/")) -- Unix-achtige systemen
print(is_directory_exists("C:\\pad\\om\\te\\controleren\\")) -- Windows systemen
```

Een voorbeelduitvoer kan simpelweg `true` zijn als de map bestaat of `false` als deze niet bestaat.

## Dieper Duiken

In de vroege computertijd was bestandsbeheer cruciaal in besturingssystemen, en het controleren van het bestaan van mappen was eenvoudig in shell-opdrachten. Lua, ontworpen om ingebed en uitgebreid te worden, blijft minimalistisch en vertrouwt dus op externe oproepen voor dergelijke taken.

Lua's `os.execute` functie voert een systeemopdracht uit, waardoor het veelzijdig is voor dit doel. Unix-gebaseerde systemen reageren goed op de `-d` vlag die mappen controleert. In Windows dient de poging om van map te veranderen met `cd` onze controle.

Er zijn alternatieven zoals de `lfs` (LuaFileSystem) bibliotheek die `lfs.attributes(path, "mode")` biedt, een robuustere en leesbaardere methode om hetzelfde te doen, maar het vereist het installeren van extra afhankelijkheden.

Om prestatieredenen kunnen directe systeemaanroepen sneller zijn dan het opnemen van een volledige bibliotheek, vooral voor eenvoudige taken zoals het controleren van het bestaan van een map. Het gebruik van `os.execute` heeft echter overhead door het creëren van een nieuw proces, dus wees voorzichtig in een strakke lus.

## Zie Ook

- LuaFileSystem documentatie: http://keplerproject.github.io/luafilesystem/manual.html
- Lua `os` bibliotheek referentie: https://www.lua.org/manual/5.4/manual.html#6.9
- "Programming in Lua" voor een dieper begrip van de taal: https://www.lua.org/pil/
