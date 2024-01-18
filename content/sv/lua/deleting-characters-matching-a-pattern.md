---
title:                "Ta bort tecken som matchar ett mönster"
html_title:           "Lua: Ta bort tecken som matchar ett mönster"
simple_title:         "Ta bort tecken som matchar ett mönster"
programming_language: "Lua"
category:             "Lua"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/lua/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Att ta bort tecken som matchar ett mönster är en vanlig operation bland programmerare. Det innebär att man söker igenom en sträng och tar bort alla tecken som matchar ett visst mönster eller villkor. Detta kan vara användbart när man behöver rensa bort oönskade tecken eller formatera data.

## Hur gör man?
För att ta bort tecken som matchar ett mönster i Lua finns det flera olika tillvägagångssätt. Nedan följer två exempel på hur det kan göras, med tillhörande kodblock och utmatning.

Exempel 1: Ta bort alla siffror från en sträng
```
-- Definiera en funktion för att ta bort siffror från en sträng
function removeDigits(str)
    -- Hitta alla siffror och ersätt dem med ett tomt tecken
    return str:gsub("%d", "")
end

-- Anropa funktionen med en sträng som innehåller både bokstäver och siffror
result = removeDigits("Det finns 42 program på datorn.")
print(result) -- Utskrift: "Det finns program på datorn."
```

Exempel 2: Ta bort alla mellanslag från en sträng
```
-- Definiera en funktion för att ta bort mellanslag från en sträng
function removeSpaces(str)
    -- Hitta alla mellanslag och ersätt dem med ett tomt tecken
    return str:gsub("%s", "")
end

-- Anropa funktionen med en sträng som innehåller mellanslag
result = removeSpaces("   Det finns   mycket   utrymme  . ")
print(result) -- Utskrift: "Detfinnsmycketutrymme."
```

## Djupdykning
Det finns flera olika metoder för att ta bort tecken som matchar ett visst mönster i Lua. Ett vanligt alternativ är att använda funktionen `string.gsub()` som används i de ovanstående kodexemplen. En annan möjlighet är att använda reguljära uttryck, även kallat 'regular expressions', för att söka och ersätta tecken enligt ett mönster. Det finns också olika varianter av funktionen `gsub()` som till exempel `string.gsub(pattern, replacement, max)` som bara ersätter ett visst antal förekomster av mönstret.

Det är också värt att notera att funktionen `gsub()` returnerar en ny sträng och lämnar den ursprungliga strängen oförändrad. Om man vill modifiera den ursprungliga strängen kan man istället använda funktionen `string.gsub()` som har en modifieringsparameter.

## Se också
- [Lua 5.4 Reference Manual](https://www.lua.org/manual/5.4/)
- [Regular Expressions in Lua](https://www.lua.org/manual/5.4/manual.html#6.4.1)
- [String Manipulation in Lua](https://www.lua.org/pil/20.html)