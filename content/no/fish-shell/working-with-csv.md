---
title:                "Fish Shell: Arbeide med csv"
simple_title:         "Arbeide med csv"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/fish-shell/working-with-csv.md"
---

{{< edit_this_page >}}

## Hvorfor

Hvis du jobber med data, spesielt innenfor regneark eller database, er sjansen stor for at du har kommet over CSV-filer. CSV står for "Comma Separated Values" og er en vanlig måte å lagre og dele tabellbaserte data på. Hvis du vil lære å behandle disse filene mer effektivt, kan Fish Shell være det perfekte verktøyet for deg.

## Hvordan

Med Fish Shell kan du enkelt behandle og manipulere CSV-filer ved hjelp av noen enkle kommandoer. Du kan bruke kommandoen "csv" sammen med andre Unix-verktøy som grep, sed og awk for å filtrere og formatere dataene dine. Her er noen eksempler på hvordan du kan bruke Fish Shell til å arbeide med CSV-filer:

```Fish Shell
# Lese en CSV-fil og se på innholdet i terminalen
fish_til_csv filnavn.csv

# Filtrere innholdet i en CSV-fil basert på et søkeord
csv filnavn.csv | grep "søkeord"

# Oppdatere en CSV-fil med nye data
echo "ny rad" >> filnavn.csv

# Formatere dataene i en CSV-fil ved hjelp av sed
csv filnavn.csv | sed 's/,/;/g'

# Opprette en ny CSV-fil med data fra en annen fil
csv filnavn.csv > ny_filnavn.csv | csv -e ny_filnavn.csv

```

## Dypdykk

Hvis du ønsker å bli enda mer effektiv med å arbeide med CSV-filer i Fish Shell, kan du ta en dypdykk i dokumentasjonen og utforske de forskjellige mulighetene som er tilgjengelige. Du kan for eksempel lese om detaljert informasjon om kommandoer som "cut", "join" og "sort", som kan være nyttige i behandlingen av CSV-filer. Du kan også lære om hvordan du kan bruke variabler og løkker for å utføre mer komplekse operasjoner på dataene dine.

## Se også

-[Fish Shell dokumentasjon](https://fishshell.com/docs/current/index.html)
-[CSV-filer: Hvordan lese, skrive og manipulere data](https://www.makeuseof.com/tag/csv-files-read-write-manipulate/)
-[10 tips for å jobbe med CSV-filer i terminalen](https://www.techradar.com/how-to/tech/10-terminal-commands-to-work-with-csv-files-1305665)