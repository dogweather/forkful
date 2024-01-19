---
title:                "Finne lengden på en streng"
html_title:           "Arduino: Finne lengden på en streng"
simple_title:         "Finne lengden på en streng"
programming_language: "Lua"
category:             "Lua"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/lua/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?

Finne lengden på en streng i Lua betyr å telle antall tegn i den gitte strengen. Dette gjør programmerere for å håndtere databehandling, manipulering av tekst eller til og med for løkkontroll i deres kode.

## Hvordan gjøre det:

Her er et par eksempler på hvordan du finner strenglengden i Lua:

```Lua
s = "Hei Verden"
print(#s)
```
Eksemplet ovenfor vil gi output `10`, som er antall tegn i strengen "Hei Verden".

```Lua
s = "Lua Programmering"
print(string.len(s))
```
I dette eksemplet bruker vi `string.len()` funksjonen til å finne lengden på strengen "Lua Programmering", som vil gi `17` som output.

## Dykk dypere:

Historisk sett, med begrensninger i tidlig datateknologi, var lengden på strenger viktig informasjon å ha når du behandlet tekstdata. Lua, selv om det er et relativt nytt språk (første utgivelse i 1993), opprettholder denne funksjonaliteten i sin standardbibliotek.

For alternativer, kan du også bruke `string.len()` funksjonen, som gir samme resultat som operatøren '#'. Det avhenger av programmererens stil å bestemme hvilken å bruke.

For implementeringsdetaljer, bruker Lua intern strenglagring som holder oversikt over strengens lengde som en intern verdi. Dette betyr at å hente lengden på en streng er en veldig rask operasjon - det er bare å hente en eksisterende verdi, ikke behøver å telle tegn individuelt.

## Se også:

For flere detaljer om behandling av strenger i Lua, sjekk ut disse lenkene:

1. Offisiell Lua 5.4 Manual: https://www.lua.org/manual/5.4/manual.html#6.4 
2. Lua-users wiki: http://lua-users.org/wiki/StringLibraryTutorial
3. StackOverflow: https://stackoverflow.com/questions/4465964/what-is-the-length-operator-in-lua