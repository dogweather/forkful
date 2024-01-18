---
title:                "Interpolering av en streng"
html_title:           "Lua: Interpolering av en streng"
simple_title:         "Interpolering av en streng"
programming_language: "Lua"
category:             "Lua"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/lua/interpolating-a-string.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?

Å interpolere en streng i Lua betyr å flette variabler eller uttrykk inn i en streng for å skape en dynamisk tekst. Dette gjøres ofte for å gjøre koden mer effektiv og lesbar, og for å unngå å skrive ut lange strenger med variasjoner.

## Slik gjør du det:

Interpolasjon av strenger i Lua er enkelt og kan gjøres på to måter. Den første metoden er å bruke ```..``` operatoren for å flette variabler eller uttrykk inn i en streng. For eksempel:

```
local navn = "John"
print("Hei, mitt navn er " .. navn)
```

Dette vil skrive ut følgende:

```
Hei, mitt navn er John
```

Du kan også bruke ```%``` symbolet for å interpolere variabler inn i en streng. For eksempel:

```
local alder = 25
print("Jeg er %d år gammel.", alder)
```

Dette vil skrive ut:

```
Jeg er 25 år gammel.
```

Merk at ```%``` symbolet må følges av en spesifisert konvertering, i dette tilfellet ```d``` for en numerisk verdi.

## Dypdykk:

Interpolasjon av strenger har vært en del av programmeringsverdenen i mange år og brukes i ulike språk som Lua, Python og Ruby. Alternativene til å interpolere strenger er å kombinere strenger manuelt eller å bruke formateringsfunksjoner, som kan være mer arbeidskrevende.

Når det kommer til implementasjonen av strenginterpolasjon i Lua, følger det enkle regler der variabler bare kan bli flettet i tekst og ikke matematiske uttrykk.

## Se også:

Hvis du vil lære mer om interpolasjon av strenger i Lua, kan du sjekke ut følgende lenker:

- [Offisiell Lua-dokumentasjon om strenginterpolasjon](https://www.lua.org/manual/5.4/manual.html#3.4.6)
- [Eksempler på interpolasjon i Lua](https://riptutorial.com/lua/example/18428/string-interpolation)