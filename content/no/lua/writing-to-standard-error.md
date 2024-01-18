---
title:                "Å skrive til standardfeil"
html_title:           "Lua: Å skrive til standardfeil"
simple_title:         "Å skrive til standardfeil"
programming_language: "Lua"
category:             "Lua"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/lua/writing-to-standard-error.md"
---

{{< edit_this_page >}}

# Hva og Hvorfor?

Å skrive til standardfeil utgang (standard error output) er en måte å informere brukeren om feil eller unntak som oppstår under kjøring av et Lua-program. Dette gjøres ved å sende en beskjed til den standardiserte feilstrømmen (stderr), som er atskilt fra den standardiserte utgangsstrømmen (stdout). Programmerere bruker dette for å gi viktig informasjon om feil til brukeren, for å hjelpe med feilsøking og feilretting.

# Hvordan gjør vi det?

Vi bruker funksjonen ```io.stderr:write()``` for å skrive til standardfeil utgang. Denne funksjonen tar inn en streng og skriver den til feilstrømmen. Her er et eksempel på hvordan dette kan se ut i praksis:

```Lua
io.stderr:write("Oops, noe gikk galt!")
```

Dette vil skrive ut meldingen "Oops, noe gikk galt!" til feilstrømmen. Merk at du må bruke "io.stderr" foran "write" for å indikere at vi skal skrive til feilstrømmen og ikke standard utgang.

Du kan også bruke funksjonen "error()" til å kaste en feil og skrive en beskjed til feilstrømmen samtidig. For eksempel:

```Lua
error("Ups, dette er en feilmelding!", 2)
```

Dette vil kaste en feil og skrive beskjeden "Ups, dette er en feilmelding!" til feilstrømmen. Merk at vi også har gitt en nummer (2) som et argument til funksjonen. Dette indikerer at feilen har oppstått på linje 2 i koden vår.

# Dypdykk

Å skrive til standardfeil utgang ble en standardisert praksis i mange programmeringsspråk på 1970-tallet. Dette var en del av en større bevegelse mot standardisering av kommunikasjon mellom programvare. Alternativet til å skrive til feilstrømmen er å skrive til standard utgang eller til en loggfil. Men å skrive til feilstrømmen gir en mer direkte og umiddelbar måte å kommunisere feil og unntak til brukeren på.

Det kan også være nyttig å vite at standardfeil utgang er en del av standardbiblioteket til Lua, noe som betyr at det alltid er tilgjengelig. Det finnes også andre måter å skrive til feilstrømmen på, som for eksempel ved hjelp av pcall-funksjonen eller debug.bug-traceback-funksjonen.

# Se også

For å lære mer om å skrive til standardfeil utgang i Lua, kan du se dokumentasjonen til Lua på deres offisielle nettside: https://www.lua.org/manual/5.4/manual.html#6.9.

For flere tips og triks om Lua-programmering, ta en titt på denne listen over "best practices": https://gist.github.com/Indo3D/624f82dd3127111fa162b03790fd31a4.