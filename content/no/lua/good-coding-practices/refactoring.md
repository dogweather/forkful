---
aliases:
- /no/lua/refactoring/
date: 2024-01-26 01:46:46.913075-07:00
description: "Refaktorisering er kunsten \xE5 justere eksisterende kode for \xE5 forbedre\
  \ strukturen, lesbarheten, og effektiviteten uten \xE5 endre dens eksterne oppf\xF8\
  rsel.\u2026"
lastmod: 2024-02-18 23:08:54.023755
model: gpt-4-0125-preview
summary: "Refaktorisering er kunsten \xE5 justere eksisterende kode for \xE5 forbedre\
  \ strukturen, lesbarheten, og effektiviteten uten \xE5 endre dens eksterne oppf\xF8\
  rsel.\u2026"
title: Refaktorering
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Refaktorisering er kunsten å justere eksisterende kode for å forbedre strukturen, lesbarheten, og effektiviteten uten å endre dens eksterne oppførsel. Programmerere gjør dette for å gjøre koden mer vedlikeholdbar, redusere kompleksitet, og ofte som et foreløpig skritt før de legger til nye funksjoner eller retter feil.

## Hvordan:
La oss ta en enkel Lua-funksjon og refaktorisere den. Vi starter med en funksjon som beregner summen av tall i en liste, men som er skrevet uten mye tanke for effektivitet eller klarhet:

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

print(sumList({1, 2, 3, 4})) -- Gir ut: 10
```

Refaktoriser til en mer effektiv og lesbar versjon:
```Lua
function sumListRefactored(numbers)
    local result = 0
    for _, value in ipairs(numbers) do
        result = result + value
    end
    return result
end

print(sumListRefactored({1, 2, 3, 4})) -- Gir fortsatt ut: 10
```

Den refaktoriserte versjonen blir kvitt den overflødige indre løkken, og bruker `ipairs` for å iterere gjennom listen på en ryddig måte.

## Dypdykk
Historisk kommer refaktorisering fra Smalltalk-programmeringsfellesskapet på slutten av 80-tallet og ble popularisert av Martin Fowlers bok 'Refaktorisering: Forbedring av designet av eksisterende kode'. I Lua innebærer refaktorisering ofte forenkling av komplekse betingelser, oppdeling av store funksjoner til mindre, og optimalisering av tabellbruk for å forbedre ytelsen.

Refaktorisering i Lua har sine forbehold; Lua sin dynamiske natur og fleksible typetilpasning kan gjøre visse refaktoriseringer, som å endre navn på variabler eller endre funksjonssignaturer, riskabelt hvis det ikke gjøres forsiktig. Verktøy for statisk kodeanalyse (som `luacheck`) kan redusere slike risikoer. Alternativer inkluderer testdrevet utvikling (TDD), hvor kode kontinuerlig refaktoriseres som en integrert del av utviklingsprosessen, i motsetning til en separat refaktoriseringsfase.

## Se også
- "Programmering i Lua" av Roberto Ierusalimschy for beste praksis og eksempler.
- "Refaktoring: Forbedring av designet av eksisterende kode" av Martin Fowler for prinsipper som gjelder på tvers av språk.
- LuaRocks-katalogen (https://luarocks.org/) for verktøy og moduler rettet mot vedlikehold og refaktorisering av Lua-kode.
