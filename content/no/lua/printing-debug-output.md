---
title:                "Utskrift av feilsøkingsresultat"
html_title:           "Arduino: Utskrift av feilsøkingsresultat"
simple_title:         "Utskrift av feilsøkingsresultat"
programming_language: "Lua"
category:             "Lua"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/lua/printing-debug-output.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?

Skriving av debug output er prosessen med å utvise kode data for feilsøking. Programmerere gjør det for å forstå hva som skjer med kodene deres under kjøring.

## Hvordan: 

Her er koden for å skrive debug output i Lua. Bruk `print()` funksjonen for dette formålet. 

```Lua
print("Hei, dette er en debugg-melding!") 
```

Når du kjører den ovennevnte koden, får du følgende utdata:

```
Hei, dette er en debugg-melding!
```

## Dyp Dykk: 

Historisk har debug output vært brukt siden de tidligste dagene av programmering. Det er viktig for å forstå hvor, når og hvordan feil eller uventede hendelser oppstår under kjøring av kode.

Alternativer til `print()` i Lua inkluderer bruk av en logger, som `logging.lua` biblioteket. Det lar deg kontrollere nivået av logger du vil skrive ut, og også der du vil ha loggene dine skrevet (for eksempel en fil).

Når det gjelder implementeringsdetaljer, bruker Lua en virtuell stackmaskin for å utføre kommandoene. Når `print()` funksjonen blir kalt, ser Lua på toppen av stakken for å hente argumentene som er lagt der, skriver ut dem, og deretter fjerner dem fra stakken.

## Se Også:

1. Lua 5.4 referanse manual: [https://www.lua.org/manual/5.4/manual.html](https://www.lua.org/manual/5.4/manual.html)
2. "Programming in Lua" av Roberto Ierusalimschy: [https://www.lua.org/pil/](https://www.lua.org/pil/)
3. Lua logging bibliotek: [https://keplerproject.github.io/lualogging/manual.html](https://keplerproject.github.io/lualogging/manual.html)