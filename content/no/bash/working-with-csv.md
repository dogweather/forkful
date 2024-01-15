---
title:                "Å jobbe med csv"
html_title:           "Bash: Å jobbe med csv"
simple_title:         "Å jobbe med csv"
programming_language: "Bash"
category:             "Bash"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/bash/working-with-csv.md"
---

{{< edit_this_page >}}

## Hvorfor?

Hvis du noen gang har jobbet med store datasett, har du sannsynligvis støtt på filformatet CSV (comma-separated values). CSV-filer er enkelt å lese og skrive, og er et vanlig format for å lagre og utveksle data. Ved å lære å jobbe med CSV-filer i Bash, kan du effektivt håndtere og manipulere store datasett på en enkel måte.

## Slik gjør du det

For å lese og skrive CSV-filer i Bash, kan du bruke kommandoen "csvtool". Denne kommandoen er en del av pakken "csvkit", som må installeres først. Du kan installere "csvkit" ved å kjøre følgende kommando i terminalen:

```Bash
sudo apt install csvkit
```

Når "csvkit" er installert, kan du begynne å bruke "csvtool". Her er noen nyttige kommandoer og eksempler:

- For å lese en CSV-fil og få utskrift på skjermen, bruk følgende kommando: 

```Bash
csvtool format '%(1:s) - %(2:s)' sample.csv
```

Dette vil skrive ut innholdet i "sample.csv" med en tilpasset formatering.

- For å velge bare spesifikke kolonner, kan du bruke flagget "-c" med en kommaseparert liste over kolonneindekser, for eksempel:

```Bash
csvtool -c 1,3,5 sample.csv
```

Dette vil bare skrive ut kolonnene 1, 3 og 5 fra "sample.csv".

- Du kan også søke etter et bestemt mønster i en bestemt kolonne ved å bruke flagget "-u" med søkeordet og kolonneindeksen, for eksempel:

```Bash
csvtool -u 'søkeord' -c 2 sample.csv
```

Dette vil søke etter "søkeord" i kolonne 2 i "sample.csv".

## Dypdykk

I tillegg til de nevnte kommandoene, har "csvtool" mange flere funksjoner, inkludert muligheten til å filtrere og sortere data, samt utføre matematiske beregninger. Du kan også bruke "csvtool" til å konvertere CSV-filer til andre formater som JSON eller HTML.

En annen måte å jobbe med CSV-filer i Bash er ved hjelp av "awk" -kommandoen. Dette er en kraftig kommando som lar deg søke, sortere og manipulere data på en lignende måte som "csvtool". For å lære mer om "awk", kan du lese denne artikkelen.

## Se også

- [En innføring i Bash](https://en.wikipedia.org/wiki/Bash_(Unix_shell))
- [Offisiell dokumentasjon for "csvkit"](https://csvkit.readthedocs.io/en/latest/)
- [En oversikt over "awk" kommandoen](https://www.grymoire.com/Unix/Awk.html)