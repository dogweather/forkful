---
title:                "Sammenligning av to datoer"
html_title:           "Lua: Sammenligning av to datoer"
simple_title:         "Sammenligning av to datoer"
programming_language: "Lua"
category:             "Lua"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/lua/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?

Å sammenligne to datoer er en metode som lar programmerere sammenligne to gitt datoer for å bestemme hvilken som kommer først eller sist. Dette kan være nyttig for å sortere datoer eller søke etter datoer innenfor en tidsramme.

## Hvordan å:

```Lua
-- Bruke os.time() for å få dagens dato og lagre den som en variabel
local iDag = os.time()

-- Opprette en annen dato som skal sammenlignes
local annenDato = os.time({year = 2021, month = 8, day = 25})

-- Sammenligne de to datoene og lagre resultatet som en variabel
local resultat = iDag - annenDato

-- Skrive ut resultatet
print(resultat)
```

Output: -56166400

## Mer inngående:

Det å sammenligne datoer har vært en del av programmering siden tidligere tider. I dag brukes det ofte for å holde orden på datoer og tidspunkter i programmer. En alternativ måte å sammenligne datoer på er ved å bruke tidssone-konverteringer, men dette kan føre til forvirring i komplekse programmer. Implementeringsdetaljer for å sammenligne datoer kan variere avhengig av språket, men felt som år, måned og dag er vanligvis inkludert.

## Se også:

- [Dato og tid i Lua](https://www.lua.org/pil/22.1.html)
- [Vanlige feil i dato og tid på programmering](https://www.oreilly.com/library/view/what-could-possibly/9780596802490/ch04s03.html)