---
title:                "Bash: Å jobbe med csv"
simple_title:         "Å jobbe med csv"
programming_language: "Bash"
category:             "Bash"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/bash/working-with-csv.md"
---

{{< edit_this_page >}}

## Hvorfor

CSV, eller Comma-Separated Values, er en vanlig måte å lagre store mengder data på. Dette kan være alt fra kontaktpersoner og forretningsinformasjon til statistikk og datasett. Å jobbe med CSV-filer kan være nyttig for å analysere og organisere data på en enkel og strukturert måte. Med Bash programming, kan du automatisere mange av de manuelle oppgavene og gjøre det lettere å håndtere store databaser.

## Hvordan

For å begynne å jobbe med CSV i Bash, trenger du å forstå de grunnleggende kommandoene som brukes til å lese og manipulere data. Her er noen enkle eksempler på hvordan du kan få tilgang til data i en CSV-fil og gjøre ulike operasjoner på den.

### Les data fra en CSV-fil

For å lese data fra en CSV-fil, kan du bruke `cat` kommandoen. For eksempel, hvis du har en fil som heter "dataset.csv", kan du bruke følgende kommando for å vise innholdet:

```Bash
cat dataset.csv
```

Dette vil vise alt innholdet fra filen, inkludert kolonner og rader med data.

### Velg spesifikke kolonner

Hvis du vil velge bare spesifikke kolonner fra din CSV-fil, kan du bruke `cut` kommandoen. For eksempel, hvis du ønsker å velge bare de første tre kolonnene fra din fil, kan du bruke følgende kommando:

```Bash
cat dataset.csv | cut -d ',' -f 1-3
```

Dette vil vise bare de første tre kolonnene fra filen, som kan hjelpe deg med å fokusere på de dataene du trenger.

### Filtrere data

For å filtrere ut data basert på en bestemt kriterie, kan du bruke `grep` kommandoen. La oss si at du bare ønsker å vise rader som inneholder ordet "kunde" i den første kolonnen. Du kan bruke følgende kommando:

```Bash
cat dataset.csv | grep '^kunde'
```

Dette vil vise alle rader som starter med ordet "kunde".

## Deep Dive

Det er mange flere kommandoer og operasjoner du kan bruke for å jobbe med CSV-filer i Bash, som `sort`, `awk`, `sed` og mange flere. For å lære mer, kan du sjekke ut dokumentasjonen og prøve dem ut selv. Det finnes også mange nyttige ressurser og tutorials tilgjengelig på nettet som kan hjelpe deg å bli mer komfortabel med å jobbe med CSV-filer i Bash.

## Se Også

- [Bash manualen på norsk](https://www.gnu.org/software/bash/manual/bash.html#Introduction-to-Globbing)
- [Kommandoer for å jobbe med CSV i Bash](https://linuxconfig.org/bash-cut-csv-file-programming-examples)