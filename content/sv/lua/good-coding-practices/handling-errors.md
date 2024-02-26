---
date: 2024-01-26 00:56:50.823597-07:00
description: "Att hantera fel i kodning \xE4r att v\xE4nta sig det ov\xE4ntade. Det\
  \ \xE4r konsten att planera f\xF6r n\xE4r saker och ting g\xE5r snett s\xE5 att\
  \ du kan h\xE5lla ditt program\u2026"
lastmod: '2024-02-25T18:49:36.359868-07:00'
model: gpt-4-1106-preview
summary: "Att hantera fel i kodning \xE4r att v\xE4nta sig det ov\xE4ntade. Det \xE4\
  r konsten att planera f\xF6r n\xE4r saker och ting g\xE5r snett s\xE5 att du kan\
  \ h\xE5lla ditt program\u2026"
title: Hantering av fel
---

{{< edit_this_page >}}

## Vad & Varför?
Att hantera fel i kodning är att vänta sig det oväntade. Det är konsten att planera för när saker och ting går snett så att du kan hålla ditt program körande smidigt.

## Hur gör man:
Lua använder två huvudfunktioner för felhantering: `pcall` och `xpcall`. Så här använder du dem:

```lua
function might_fail()
    if math.random() > 0.5 then
        error("Hoppsan! Något gick fel.")
    else
        print("Allt är bra!")
    end
end

-- Använda pcall
local success, errorMessage = pcall(might_fail)

if success then
    print("Lyckades!")
else
    print("Ett fel uppstod:", errorMessage)
end

-- Använda xpcall med en felhanterare
function myErrorHandler(err)
    print("Felhanteraren säger:", err)
end

local status = xpcall(might_fail, myErrorHandler)
print("Var anropet framgångsrikt?", status)
```

Exempel på utskrift kan vara:

```
Ett fel uppstod: Hoppsan! Något gick fel.
Felhanteraren säger: Hoppsan! Något gick fel.
Var anropet framgångsrikt? false
```
Eller, om inget fel inträffar:
```
Allt är bra!
Lyckades!
Allt är bra!
Var anropet framgångsrikt? true
```

## Fördjupning
Att hantera fel, eller "exceptionhantering", var inte alltid en sak. Tidiga program kraschade – mycket. Med kodningens utveckling växte även behovet av stabilitet. Luas tillvägagångssätt är enkelt jämfört med vissa språk. Det finns inga `try/catch` block, bara `pcall` och `xpcall`. Den första skyddar ett funktionsanrop, och returnerar en status och eventuellt fel. Den andra lägger till en felhanteringsfunktion, användbar för anpassad städning eller loggning.

Ett alternativ i Lua är att använda `assert`, som kan tjäna ett liknande syfte genom att kasta ett fel om dess villkor är falskt. Men det är inte lika flexibelt som `pcall` för komplexa felhanteringsscenarier.

Internt fungerar `pcall` och `xpcall` genom att ställa in en "skyddad miljö" för funktionen att köras i. Om ett fel dyker upp fångar miljön det och kan antingen hantera det direkt eller skicka tillbaka det för programmet att hantera.

## Se även
- Boken Programming in Lua (tredje upplagan), tillgänglig på https://www.lua.org/pil/ för grundlig läsning om felhantering (Sektion 8.4).
- Officiell Lua 5.4 referensmanual: https://www.lua.org/manual/5.4/ - för den mest uppdaterade informationen om Luas felhanteringsfunktioner.
- Lua-användares wiki om felhantering: http://lua-users.org/wiki/ErrorHandling – för insikter och mönster från communityn.
