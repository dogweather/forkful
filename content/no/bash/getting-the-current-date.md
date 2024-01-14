---
title:                "Bash: Få dagens dato"
simple_title:         "Få dagens dato"
programming_language: "Bash"
category:             "Bash"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/bash/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Hvorfor
Det å hente dagens dato er en vanlig oppgave i mange programmeringsprosjekter. Datoen kan brukes til å printe ut på skjermen, lagre i en fil eller brukes til å organisere data på en mer intuitiv måte. Uansett hva årsaken måtte være, er det viktig å kunne hente dagens dato på en enkel og effektiv måte.

## Hvordan
For å hente dagens dato i et Bash-skript, kan du bruke kommandoen `date +%Y-%m-%d`. Dette vil gi deg datoen i formatet "år-måned-dag". For eksempel, hvis du kjører kommandoen i dag vil du få 2021-10-21.

```Bash
#!/bin/bash
today=$(date +%Y-%m-%d)
echo "Dagens dato er $today"
```

Skriptet over vil printe ut "Dagens dato er 2021-10-21" når det blir kjørt.

Hvis du ønsker å inkludere ukedagen i datoen, kan du bruke `date +%A` kommandoen, som vil gi deg navnet på ukedagen på ditt lokale språk. Hvis du vil ha datoen i et annet format, kan du se på `man date` for en liste over alle tilgjengelige formater.

## Dypdykk
En interessant funksjon ved `date` kommandoen er muligheten til å legge til eller trekke fra en viss tid til den nåværende datoen. Dette kan være nyttig hvis du for eksempel ønsker å hente datoen en uke fra i dag eller en måned tilbake i tid. Du kan gjøre dette ved å bruke `date -d` kommandoen, etterfulgt av et tall og en tidsenhet.

```Bash
#!/bin/bash
next_week=$(date -d "+1 week" +%Y-%m-%d)
echo "Neste uke er $next_week"
```

Skriptet over vil printe ut "Neste uke er 2021-10-28" hvis det blir kjørt i dag.

## Se også
- [Offisiell dokumentasjon for `date` kommandoen](https://man7.org/linux/man-pages/man1/date.1.html)
- [En kort og enkel guide for å hente dagens dato i Bash](https://www.cyberciti.biz/faq/unix-linux-get-the-todays-date/)
- [En dypere forklaring på hvordan `date` kommandoen fungerer på Linux](https://www.baeldung.com/linux/date-command)