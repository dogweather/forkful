---
title:                "Å bruke en feilsøker"
aliases:
- /no/lua/using-a-debugger.md
date:                  2024-01-26T03:50:18.805776-07:00
model:                 gpt-4-0125-preview
simple_title:         "Å bruke en feilsøker"

tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/lua/using-a-debugger.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
En feilsøker er et verktøy som lar deg inspisere og kontrollere utførelsen av et program, noe som gjør det enkelt å finne ut hvor ting går galt. Programmerere bruker feilsøkere til å knuse feil, forstå kodeflyt, og for å sikre at koden deres er så ren som en fløyte.

## Hvordan:
Lua kommer ikke med en innebygd feilsøker, men du kan bruke eksterne feilsøkere, som ZeroBrane Studio. Her er en smakebit på hvordan du ville jobbet med det:

```Lua
-- Dette er et enkelt Lua-skript med en med hensikt lagt til feil
local function add(a, b)
    local result = a + b -- Oops, la oss late som vi glemte å definere 'b'
    return result
end

print(add(10))
```

Når du kjører dette i en feilsøker, vil den stoppe utførelsen der ting roter seg til. Du vil se noe slik:

```
lua: example.lua:3: forsøk på å utføre aritmetikk på en nil-verdi (lokal 'b')
stack traceback:
	example.lua:3: i funksjon 'add'
	example.lua:7: i hovedstykket
	[C]: i ?
```

Du kan sette brytepunkter, steg-for-steg gå gjennom koden din, og ta en titt på variabelverdier for å spore opp feilen uten å miste forstanden.

## Dypdykk
Luas enkelhet strekker seg dessverre ikke til feilsøking. Ingen bekymringer, dog; Lua-samfunnet har ryggen din. Verktøy som ZeroBrane Studio, LuaDec, og andre tilbyr feilsøkingskapasiteter. Historisk sett, eksisterte feilsøkere ikke lenge etter at de første programmene gikk surt, og ga utviklere muligheten til å rette koden sin uten å blindt fikle rundt.

Med Lua, er du ofte avhengig av eksterne feilsøkere eller bygger dem inn i ditt utviklingsmiljø. ZeroBrane Studio, for eksempel, er en IDE som fullt ut integrerer en Lua-feilsøker. Den lar deg steg-for-steg gå gjennom koden, sette brytepunkter, og overvåke variabler. På implementeringssiden, bruker feilsøkere vanligvis kroker for å sette inn brytepunkter og andre feilsøkingstilbud.

Alternativer? Absolutt. Gode gamle `print` uttalelser, kjærlig kjent som "printf-feilsøking," kan noen ganger gjøre susen uten fancy verktøy.

## Se Også
For å fortsette din feilsøkingsreise, sjekk ut:

- ZeroBrane Studio: https://studio.zerobrane.com/
- Lua-brukeres wiki om feilsøking av Lua-kode: http://lua-users.org/wiki/DebuggingLuaCode
- `debug` bibliotekreferanse i Luas håndbok: https://www.lua.org/manual/5.4/manual.html#6.10
