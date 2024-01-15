---
title:                "Oversette en dato til en streng"
html_title:           "Bash: Oversette en dato til en streng"
simple_title:         "Oversette en dato til en streng"
programming_language: "Bash"
category:             "Bash"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/bash/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Hvorfor

Å konvertere en dato til en streng er et viktig verktøy i Bash-programmering. Det lar deg presentere datoer på en leserlig måte og gjøre de enklere å behandle i programmene dine.

## Hvordan

For å konvertere en dato til en streng i Bash, kan du bruke kommandoen `date` sammen med formateringsalternativet `+%m/%d/%Y` som vil gi datoen i måned/dag/år format. Her er et eksempel på kode og tilhørende utdata:

```Bash
date +%m/%d/%Y
```

Utdata:

```
08/25/2021
```

Dette formateringsalternativet kan også kombineres med andre alternativer for å tilpasse utdataen etter dine behov. Ta en titt på Bash-dokumentasjonen for å utforske ulike muligheter.

## Dypdykk

Datoformatering kan virke forvirrende, men ved å forstå noen enkle konsepter, kan du raskt lære hvordan du konverterer en dato til en streng i Bash-programmering. Biporumsjonen `%m` brukes for å få måneden med to sifre, `%d` for å få dagen med to sifre og `%Y` for å få året med fire sifre. Du kan også bruke `%b` for å få måneden i forkortet form og `%B` for å få den i full form.

Det finnes også andre forhåndsdefinerte alternativer som `%a` for å få ukedagen i forkortet form og `%A` for å få den i full form. Du kan også legge til en ` ` (mellomrom) eller `-` (bindestrek) etter prosenttegnet for å få spaces eller bindestrek mellom dato elementene. Se på noen eksempler under:

| Alternativ | Utput  | Eksempel |
| ---------- | ------ | --- |
| `%m/%d/%Y` | `08/25/2021` | Dagens dato |
| `%B %d, %Y` | `August 25, 2021` | Dagens dato |
| `%a, %d %b` | `Wed, 25 Aug` | Dagens dato |
| `%d-%m-%Y` | `25-08-2021` | Dagens dato |

Det er også mulig å kombinere flere alternativer for å få en mer spesifikk datoformat. For eksempel kan du bruke `%B %d, %Y %H:%M` for å få følgende utdata: `August 25, 2021 09:45`.

## Se også

- [Bash dokumentasjon](https://www.gnu.org/software/bash/manual/bash.html)
- [Stack Overflow: How to convert date to string in bash](https://stackoverflow.com/questions/1401482/how-to-convert-date-to-string-in-bash)