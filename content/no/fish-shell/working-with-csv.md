---
title:                "Arbeide med csv"
html_title:           "Fish Shell: Arbeide med csv"
simple_title:         "Arbeide med csv"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/fish-shell/working-with-csv.md"
---

{{< edit_this_page >}}

## Hvorfor 
Hvorfor bry seg om å jobbe med CSV? Vel, CSV (Comma Separated Values) er et populært filformat for å lagre tabellarisk data. Det brukes ofte til å importere og eksportere data mellom ulike programmer og systemer. Ved å lære hvordan du jobber med CSV i Fish Shell, kan du enkelt behandle store mengder data og automatisere oppgaver.

## Slik gjør du det 
Å jobbe med CSV-filer i Fish Shell er enkelt og effektivt. Her er noen eksempler på hvordan du kan gjøre det:

```
# Hente data fra en CSV-fil og skrive ut det første elementet i hver rad
set input (csv from-file data.csv)
echo $input[1]

# Legge til en ny rad i en eksisterende CSV-fil
set data "navn, alder, kjønn"
set data "$data\nJohn, 30, Mann"
csv into-file --no-header --overwrite data.csv $data 

# Filtere data fra en CSV-fil basert på et kriterie og skrive resultatet til en ny fil
set input (csv from-file data.csv)
set filter (contains $input "Kvinne")
set output (csv into-string (sed $filter $input))
csv into-file --before "$filter" kvinner.csv $output
```

I det første eksempelet henter vi data fra en CSV-fil og skriver ut det første elementet i hver rad. Merk at vi bruker parenteser rundt kommandoen `csv from-file` for å få resultatet som en liste av rader.

I det andre eksempelet legger vi til en ny rad i en eksisterende CSV-fil. Vi lagrer først en variabel med kolonneoverskriftene og deretter legger vi til en ny rad ved å bruke `\n` for å skille radene. Til slutt bruker vi kommandoen `csv into-file` for å skrive dataene til filen.

I det siste eksempelet filtrerer vi data fra en CSV-fil basert på et kriterie og lagrer resultatet i en ny fil. Vi bruker kommandoen `csv from-file` for å hente data fra filen, deretter bruker vi `contains` for å finne alle rader som inneholder ordet "Kvinne". Til slutt bruker vi kommandoen `csv into-string` for å konvertere resultatet til en streng, og `csv into-file` for å skrive dataene til en ny fil.

## Dypdykk 
Hvis du ønsker å lære mer om hvordan du jobber med CSV i Fish Shell, kan du se nærmere på kommandoen `help csv`. Her finner du en oversikt over alle tilgjengelige kommandoer og hvordan de brukes.

For å kunne arbeide med CSV-filer i Fish Shell, må du kanskje installere et tillegg som støtter denne funksjonaliteten. Ta en titt på [fisherman](https://github.com/fisherman/fisherman) eller [oh-my-fish](https://github.com/oh-my-fish/oh-my-fish) for å finne ut hvordan du kan installere denne kommandoen og mange andre nyttige tillegg til Fish Shell.

## Se også 
- [Fish Shell dokumentasjon](https://fishshell.com/docs/current/index.html)
- [CSV-formatet på Wikipedia](https://no.wikipedia.org/wiki/Comma-separated_values)