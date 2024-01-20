---
title:                "Konvertere en streng til små bokstaver"
html_title:           "Arduino: Konvertere en streng til små bokstaver"
simple_title:         "Konvertere en streng til små bokstaver"
programming_language: "Lua"
category:             "Lua"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/lua/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?

Lurer du på hva det betyr å konvertere en streng til små bokstaver? Det handler bare om å endre alle store bokstaver i en tekststeng til små bokstaver. Programmere gjør dette for å forenkle dataanalyse og -håndtering, siden det fjerner problemer med bokstavstørrelse.

## Hvordan gjøre det:

Bruk funksjonen `string.lower()` i Lua for å konvertere en streng til små bokstaver. Her er en kort kodeeksempel:

```Lua
tekst = "Hei, Verden!"
små_bokstaver = string.lower(tekst)
print(små_bokstaver)
```

Når du kjører denne koden, blir utskriften:

```Lua
"hei, verden!"
```

## Dypdykk

Funksjonen `string.lower()` har vært tilgjengelig siden de tidlige versjonene av Lua og har vært en standardfunksjon i strengbiblioteket. Som et alternativ kan du implementere din egen funksjon ved hjelp av strengmanipulasjonsfunksjoner hvis du vil ha mer kontroll over prosessen.

Implementasjonen av `string.lower()` er ganske rett frem. Funksjonen går gjennom hver tegn i strengen, og hvis tegnet er en stor bokstav, endres det til en liten bokstav. Det gjøres ved å legge til en spesifikk verdi (32) til ASCII-verdien til det store bokstavtegnet, noe som resulterer i den tilsvarende lille bokstaven.

## Se også:

1. Lua 5.3 Reference Manual - Strings Library: http://www.lua.org/manual/5.3/manual.html#6.4
2. Programmering i Lua - Strengmanipulasjon: https://www.lua.org/pil/20.html