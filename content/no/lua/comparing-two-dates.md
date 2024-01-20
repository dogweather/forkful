---
title:                "Sammenligner to datoer"
html_title:           "Clojure: Sammenligner to datoer"
simple_title:         "Sammenligner to datoer"
programming_language: "Lua"
category:             "Lua"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/lua/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?

Sammenligning av to datoer handler om å avgjøre hvilken av de to datoene som kommer først. Dette gjør programmerere for å håndtere hendelser basert på tid, som for eksempel å finne ut tidsforskjellen mellom to hendelser.

## Slik Gjør Du:

Lua har ikke en innebygd dato- og tidsfunksjon slik andre språk har, men å sammenligne datoer kan oppnås ved hjelp av 'os' biblioteket. Her er en eksempelkode:

```lua
tip1 = os.time{year=2022, month=12, day=31, hour=23, min=59, sec=59}
t1 = os.date('*t', tip1)

tip2 = os.time{year=2022, month=11, day=30, hour=23, min=59, sec=59}
t2 = os.date('*t', tip2)

if(os.difftime(tip1, tip2) > 0) then
    print("Date1 is later than Date2")
else
    print("Date2 is later than Date1")
end
```

Når du kjører koden vil den returnere "Date1 is later than Date2".

## Dypdykk

Historisk sett, før 'os' biblioteket ble lagt til i Lua, benyttet utviklere seg av eksterne biblioteker for å håndtere datoer og tid. Men nå, med 'os' biblioteket, kan vi enkelt sammenligne to datoer.

Alternativt kan vi også bruke kommandoene `os.date` og `os.time` for å oppnå det samme. `os.date` tar en tid og konverterer det til en mer lesbar format, mens `os.time` tar en dato og konverterer det til sekunder siden EPOC.

Dato-sammenligningslogikken er veldig grunnleggende - det er et spørsmål om å konvertere datoene til sekunder og så sammenligne dem.

## Se Også

For mer informasjon, følg disse koblingene: 
1. [Lua 'os' Library](https://www.lua.org/pil/22.1.html)
3. [Lua Users Wiki](http://lua-users.org/wiki/OsLibraryTutorial)