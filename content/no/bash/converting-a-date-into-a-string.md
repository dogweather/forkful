---
title:                "Bash: Konvertere en dato til en streng"
programming_language: "Bash"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/bash/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

Hvorfor: Hvorfor konvertere dato til streng? 

Det kan være mange grunner til å konvertere en dato til en streng i Bash-programmering. Noen ganger trenger man å endre formateringen på en dato for å vise den på en annen måte, eller kanskje man ønsker å legge til en dato på slutten av en filnavn. Uansett årsak, er det viktig å vite hvordan man gjør det på riktig måte.

Slik gjør du det:

For å konvertere en datoen til en streng, kan man bruke kommandoen "date" i Bash. Her er en enkel syntaks for å konvertere datoen til et format med år-måned-dag:

```Bash
dato=$(date +"%Y-%m-%d")
echo $dato
```
Dette vil gi følgende utdata:

```Bash
2021-08-30
```

Man kan også legge til andre elementer, som for eksempel dag i uken eller tidspunkt, ved å justere uttrykket i "date" kommandoen. Her er noen eksempler:

```Bash
dato_med_tid=$(date +"%Y-%m-%d %H:%M:%S")
dag_i_uken=$(date +"%A")
```
For å bruke datoen som en del av et filnavn, kan man bruke den konverterte datoen i en variabel som er en del av filnavnet. Her er et eksempel:

```Bash
filnavn="logg_$(dato).txt"
echo $filnavn
```
Dette vil gi følgende utdata:

```Bash
logg_2021-08-30.txt
```

Dypdykk:

Det finnes mange forskjellige formateringsmuligheter for å konvertere datoer til strenger i Bash. For å se en liste over alle de tilgjengelige formatene, kan man skrive "man date" i terminalen. Her vil man finne informasjon om flagg som kan legges til i "date" kommandoen for å få ønsket format på datoen.

En annen måte å konvertere dato til streng på er å bruke funksjonen "strftime". Denne funksjonen gir også flere muligheter for hvordan man ønsker å formatere datoen.

Se også:

- [Bash dokumentasjon om "date"](https://www.gnu.org/software/coreutils/manual/html_node/date-invocation.html)
- [Mer informasjon om "strftime" funksjonen](https://www.tutorialspoint.com/strftime-function-in-bash-with-examples)
- [En guide for å formatere datoer og tider i Bash](https://likegeeks.com/bash-date-format/)