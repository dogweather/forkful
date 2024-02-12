---
title:                "Få dagens dato"
aliases:
- /no/bash/getting-the-current-date.md
date:                  2024-02-03T19:08:47.967157-07:00
model:                 gpt-4-0125-preview
simple_title:         "Få dagens dato"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/bash/getting-the-current-date.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Å hente dagens dato i Bash involverer bruk av innebygde kommandoer for å vise dato og tid i forskjellige formater. Programmerere bruker denne funksjonaliteten til oppgaver som å tidsstemple logger, planlegge oppgaver, eller bare som en del av sine systeminformasjonsskript for å spore når handlinger ble utført.

## Hvordan:
I Bash er `date`-kommandoen ditt primære verktøy for å få tak i dagens dato og tid. Her er noen eksempler på hvordan du kan bruke den:

1. **Få dagens dato og tid i standardformatet:**

```bash
date
```

*Eksempel på utdata:*
```
Wed Apr 5 14:22:04 PDT 2023
```

2. **Tilpass utdataformatet:** Du kan spesifisere utdataformatet ved å bruke `+%` format-spesifikatorer. For eksempel, for å vise datoen i YYYY-MM-DD format:

```bash
date "+%Y-%m-%d"
```

*Eksempel på utdata:*
```
2023-04-05
```

3. **Få det nåværende UNIX tidsstempelet:** UNIX-tidsstempelet er antallet sekunder siden Unix Epoch (1. januar 1970). Dette er nyttig for skript som utfører beregninger basert på tidsforskjeller.

```bash
date "+%s"
```

*Eksempel på utdata:*
```
1672877344
```

Ingen populære tredjepartsbiblioteker brukes vanligvis til denne grunnleggende operasjonen i Bash, ettersom den innebygde `date`-kommandoen gir omfattende funksjonalitet. Imidlertid, for mer avanserte dato- og tidsmanipulasjoner, kan programmerere bruke andre programmeringsspråk eller verktøy som tilbyr biblioteker for datoaritmetikk og parsing, som for eksempel Pythons `datetime`-modul.
