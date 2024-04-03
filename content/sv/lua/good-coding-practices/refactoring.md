---
date: 2024-01-26 01:47:20.418807-07:00
description: "Hur man g\xF6r: L\xE5t oss ta en enkel Lua-funktion och refaktorisera\
  \ den. Vi b\xF6rjar med en funktion som ber\xE4knar summan av talen i en lista men\
  \ \xE4r skriven utan\u2026"
lastmod: '2024-03-13T22:44:38.046708-06:00'
model: gpt-4-0125-preview
summary: "L\xE5t oss ta en enkel Lua-funktion och refaktorisera den."
title: Refaktorisering
weight: 19
---

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
