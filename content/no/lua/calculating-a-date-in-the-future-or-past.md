---
title:                "Beregning av en dato i fremtiden eller fortiden"
html_title:           "Lua: Beregning av en dato i fremtiden eller fortiden"
simple_title:         "Beregning av en dato i fremtiden eller fortiden"
programming_language: "Lua"
category:             "Lua"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/lua/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?

Å beregne en dato i fremtiden eller fortiden går ut på å ville finne ut nøyaktig hva datoen vil være på et gitt punkt i fremtiden eller hva den var på et tidspunkt i fortiden. Dette er noe programmerere gjør ganske ofte, enten det dreier seg om å håndtere forfallsdatoer, systemlogger eller tidslinjer for prosjekter.

## Hvordan

Å finne en fremtidig eller tidligere dato i Lua er en direkte prosessretning. Her er et grunnleggende eksempel ved bruk av `os.time()` og `os.date()`funksjonene:

```Lua
dagen = os.time({year=os.date("%Y"), month=os.date("%m"), day=os.date("%d")})
fremtidig_dato = os.date("*t", dagen + (24 * 60 * 60 * 7)) -- Legger til syv dager

print(fremtidig_dato.year, "-", fremtidig_dato.month, "-", fremtidig_dato.day)
```

Hvis du kjører dette vil du få datoen en uke fra i dag:

`2022 - 3 - 19`

## Dypdykk 

Historisk sett, før datamaskinenes tid, å beregne en dato i fremtiden eller fortiden var en manuell prosess, noe som lett fører til feil og unøyaktigheter.

Alternativt kan man bruke Lua's innebygde biblioteker som `os.date()` og `os.time()`, som håndterer tidssoneproblemer og skuddår for deg noe som forenkler hele prosessen.

Når det kommer til implementering, er dette egentlig bare en beregning basert på sekunder. `os.time()` gir oss det nåværende tidspunktet som et tid-stempel (antall sekunder siden 1. januar 1970), og du kan legge til eller trekke sekunder for å beregne fremtiden eller fortiden.

## Se Også

For mer informasjon om Lua og dato/tidsmanipulasjon, sjekk ut de følgende ressursene:

1. Lua's offisielle dokumentasjon: https://www.lua.org/pil/22.1.html
2. OCDive's Guide to Lua's date og time funksjoner: https://ocdoc.cil.li/api:os_time_and_date
3. Roblox's forklaring og bruk av `os.time()` og `os.date()`: https://developer.roblox.com/en-us/api-reference/lua-docs/os