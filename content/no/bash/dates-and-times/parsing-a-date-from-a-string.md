---
title:                "Analysering av en dato fra en streng"
date:                  2024-02-03T19:13:41.965206-07:00
model:                 gpt-4-0125-preview
simple_title:         "Analysering av en dato fra en streng"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/bash/parsing-a-date-from-a-string.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Hva & Hvorfor?

Å tolke en dato fra en tekststreng i Bash innebærer å ekstrahere og konvertere datoinformasjon fra tekstuelle data til et format som Bash kan manipulere eller bruke til videre prosesser. Dette er et vanlig krav i skripting for oppgaver som loggfilanalyse, filorganisering basert på datostempler, eller automatisert rapportering, noe som gjør det til en essensiell ferdighet for programmerere for å håndtere og utnytte tidsrelaterte data effektivt.

## Hvordan:

Bash i seg selv er ganske begrenset i direkte datotolkningsmuligheter, ofte avhenger den av eksterne verktøy som `date` og `awk` for mer sofistikert manipulasjon. Her er hvordan du kan tolke et spesifikt format og deretter bruke det med `date`-kommandoen for å konvertere det eller utføre operasjoner.

**Eksempel 1:** Uttrække en datostreng og konvertere den til et annet format.

Anta at du har en dato i formatet `åååå-mm-dd` og du ønsker å konvertere den til `dd-mm-åååå`.

```bash
original_dato="2023-04-01"
formatert_dato=$(date -d $original_dato '+%d-%m-%Y')

echo $formatert_dato
```

**Eksempel på utdata:**
```
01-04-2023
```

Dette bruker `date`-kommandoen med `-d`-alternativet for å spesifisere inndato-strengen, og `+%d-%m-%Y` for å formatere utdata.

**Eksempel 2:** Bruke `awk` for å tolke en dato fra en strukturert tekstlinje og konvertere den.

Anta at du har en loggfillinje: 

```
2023-04-01 12:00:00 Bruker logget inn
```

Du kan ekstrahere og konvertere datodelen ved å bruke `awk` og `date`.

```bash
logg_linje="2023-04-01 12:00:00 Bruker logget inn"
datodel=$(echo $logg_linje | awk '{print $1}')
formatert_dato=$(date -d $datodel "+%A, %B %d, %Y")

echo $formatert_dato
```

**Eksempel på utdata:**
```
Lørdag, april 01, 2023
```

Dette eksemplet bruker `awk` for å splitte logglinjen og ekstrahere datodelen (`$1` representerer det første mellomrom-delt feltet), og deretter brukes `date` til å omformatere den.

### Bruk av tredjepartsverktøy

For mer kompleks tolkning eller når man har å gjøre med en lang rekke datofortmatter, kan tredjepartsverktøy som `dateutils` være veldig nyttig.

**Eksempel med `dateutils`:**

Anta at du har en datostreng i et ikke-standard format, for eksempel, `April 01, 2023`.

```bash
original_dato="April 01, 2023"
formatert_dato=$(dateconv -i "%B %d, %Y" -f "%Y-%m-%d" <<< $original_dato)

echo $formatert_dato
```

**Eksempel på utdata:**
```
2023-04-01
```

Denne kommandoen bruker `dateconv` fra `dateutils`, og spesifiserer inndataformatet med `-i` og ønsket utdataformat med `-f`. `dateutils` støtter et stort utvalg av dato- og tidsformater, noe som gjør det svært allsidig for dato-tolkningsoppgaver i Bash-skript.