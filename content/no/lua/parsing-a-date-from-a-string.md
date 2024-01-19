---
title:                "Tolke en dato fra en streng"
html_title:           "Bash: Tolke en dato fra en streng"
simple_title:         "Tolke en dato fra en streng"
programming_language: "Lua"
category:             "Lua"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/lua/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Å analysere en dato fra en streng innebærer å hente ut informasjon om dato, måned og år fra en strengtekst. Dette gjør programmerere for å manipulere data, gjøre beregninger, utføre funksjoner som sortering og filtrering, og tilby mer tilpasset brukeropplevelse i programmer og applikasjoner.

## Hvordan gjør man det:
Her er en enkel måte å analysere en dato fra en string i Lua. Koden er skrevet i Lua 5.4.2, som er den seneste versjonen av Lua ved skrivetidspunktet.

```Lua
dato_streng = "2022-04-18"
dato, måned, år = dato_streng:match("(%d+)-(%d+)-(%d+)")
print("Dato: "..dato.." - Måned: "..måned.." - År: "..år)
```
Når du kjører dette skriptet, vil utdataene være:

```
Dato: 18 - Måned: 04 - År: 2022
```

## Dypdykk
Datoanalyse har alltid vært en viktig del av programmering, og det har gått gjennom flere iterasjoner og metoder for implementering. I tidlige programmeringsspråk ble dette ofte gjort manuelt, men moderne språk som Lua tilbyr innebygde funksjoner for å lette prosessen.

Det finnes også alternative måter å analysere en dato på fra en streng i Lua, som å bruke funksjoner fra biblioteker som `os.date` og `os.time` hvis du jobber med mer komplekse dato- og tidsformater.

Så langt vi har sett, utfører Lua match-funksjonen ovenfor sammenligning på tegnnivå for å finne korresponderende verdier. Som sådan er det viktig å sørge for at datoformatet i strengen samsvarer med mønsteret du har gitt i match-funksjonen.

## Se Også
For mer detaljert informasjon om hvordan håndtere datoer og tider i Lua, sjekk ut følgende kilder:

1. Programming in Lua (4. utgave) av Roberto Ierusalimschy
2. Lua-Users Wiki: http://lua-users.org/wiki/OsLibraryTutorial
3. Lua Documentation: https://www.lua.org/manual/5.4/manual.html#6.9