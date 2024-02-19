---
aliases:
- /nl/lua/removing-quotes-from-a-string/
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:06:26.877562-07:00
description: "Het verwijderen van aanhalingstekens uit een tekenreeks betekent dat\
  \ je die dubbele of enkele aanhalingstekens die je tekst omarmen, wegpeelt.\u2026"
lastmod: 2024-02-18 23:09:01.978168
model: gpt-4-0125-preview
summary: "Het verwijderen van aanhalingstekens uit een tekenreeks betekent dat je\
  \ die dubbele of enkele aanhalingstekens die je tekst omarmen, wegpeelt.\u2026"
title: Quotes verwijderen uit een string
---

{{< edit_this_page >}}

## Wat & Waarom?
Het verwijderen van aanhalingstekens uit een tekenreeks betekent dat je die dubbele of enkele aanhalingstekens die je tekst omarmen, wegpeelt. Programmeurs doen dit om invoer te saneren, het parsen te vergemakkelijken of om gegevens die misschien inconsistent worden geciteerd, te harmoniseren.

## Hoe:
Zo kun je die aanhalingstekens in Lua naar de stoeprand schoppen:

```lua
local function remove_quotes(str)
  return (str:gsub("^%p(.*)%p$", "%1"))
end

print(remove_quotes('"Hallo, Wereld!"'))     -- Hallo, Wereld!
print(remove_quotes("'Vaarwel, Aanhalingstekens!'"))  -- Vaarwel, Aanhalingstekens!
```

Bingo! Die aanhalingstekens verdwenen als sokken in een droger.

## Diepgaande Duik
Mensen schrobben al eeuwen aanhalingstekens van tekenreeksen sinds talen tekst konden verwerken, wat zo'n beetje voor altijd is. In Lua, doet de `gsub` functie het zware werk, met patronen als een scalpel om aanhalingstekens te verwijderen. Alternatieven? Zeker, je zou regex kunnen gebruiken in talen die het ondersteunen, of je eigen lus schrijven die elk karakter doorneemt (gaap, maar hé, het is jouw tijd).

Lua's patroonmatching geeft je de kracht van een regex-lite ervaring zonder een hele bibliotheek te importeren. De caret (`^`) en het dollarteken (`$`) komen overeen met respectievelijk het begin en het einde van de tekenreeks; `%p` komt overeen met elk leestekensymbool. Na het afschudden van de begin- en eindpunctuatie, vangen we al het andere op met `(.*),` en vervangen de volledige overeenkomst met die vanggroep met behulp van `" %1"`.

Onthoud wel dat Lua's patroonmatching niet zo krachtig is als volwaardige regex-motoren - bijvoorbeeld, het kan niet tellen of teruglopen. Deze eenvoudigheid is zowel een zegen als een vloek, afhankelijk van welke aanhalingstekens je aan het wringen bent en waar ze zich verstoppen.

## Zie Ook
Duik dieper in Lua’s patroonmatching met het PiL (Programming in Lua) boek: http://www.lua.org/pil/20.2.html

Voor pure elegantie, kijk hoe andere talen dit doen ter vergelijking, beginnend met Python’s `str.strip`: https://docs.python.org/3/library/stdtypes.html#str.strip
