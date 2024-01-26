---
title:                "Refaktorisering"
date:                  2024-01-26T01:47:20.418807-07:00
model:                 gpt-4-0125-preview
simple_title:         "Refaktorisering"
programming_language: "Lua"
category:             "Lua"
tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/lua/refactoring.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Refaktorisering är konsten att justera befintlig kod för att förbättra dess struktur, läsbarhet och effektivitet utan att ändra dess externa beteende. Programmerare gör det för att göra sin kod mer underhållbar, minska komplexiteten och ofta som ett förberedande steg innan de lägger till nya funktioner eller fixar buggar.

## Hur man gör:
Låt oss ta en enkel Lua-funktion och refaktorisera den. Vi börjar med en funktion som beräknar summan av talen i en lista men är skriven utan mycket tanke på effektivitet eller klarhet:

```Lua
function sumList(numbers)
    local result = 0
    for i=1, #numbers do
        for j=1, #numbers do
            if i == j then
                result = result + numbers[i]
            end
        end
    end
    return result
end

print(sumList({1, 2, 3, 4})) -- Ger ut: 10
```

Refaktorisera till en mer effektiv och läsbar version:
```Lua
function sumListRefactored(numbers)
    local result = 0
    for _, value in ipairs(numbers) do
        result = result + value
    end
    return result
end

print(sumListRefactored({1, 2, 3, 4})) -- Ger fortfarande ut: 10
```

Den refaktoriserade versionen blir av med den onödiga inre loopen, och använder `ipairs` för att iterera genom listan på ett rent sätt.

## Djupdykning
Historiskt kommer refaktorisering från Smalltalk-programmeringsgemenskapen i slutet av 80-talet och populariserades av Martin Fowlers bok "Refactoring: Improving the Design of Existing Code". I Lua innebär refaktorisering ofta att förenkla komplexa villkor, bryta ned stora funktioner till mindre, och optimera table-användning för att förbättra prestanda.

Refaktorisering i Lua har sina fallgropar; Luas dynamiska natur och flexibla typning kan göra vissa refaktoriseringar, som att byta namn på variabler eller ändra funktionssignaturer, riskabla om de inte görs försiktigt. Verktyg för statisk kodanalys (som `luacheck`) kan minska sådana risker. Alternativ inkluderar testdriven utveckling (TDD), där kod kontinuerligt refaktoriseras som en integrerad del av utvecklingsprocessen, i kontrast till en separat refaktoriseringsfas.

## Se även
- "Programming in Lua" av Roberto Ierusalimschy för bästa praxis och exempel.
- "Refactoring: Improving the Design of Existing Code" av Martin Fowler för principer som är tillämpliga över språk.
- LuaRocks-katalogen (https://luarocks.org/) för verktyg och moduler avsedda för underhåll och refaktorisering av Lua-kod.