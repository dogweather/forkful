---
title:    "Bash: Få gjeldende dato"
keywords: ["Bash"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/no/bash/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Hvorfor

Å få den nåværende datoen kan virke som en enkel og fremfor alt unødvendig oppgave. Men som enhver programmerer vet, er nøyaktighet og effektivitet viktig i koding. Å kunne hente den nøyaktige datoen kan være avgjørende for å utføre en bestemt operasjon eller for å sikre at et program fungerer riktig.

## Hvordan

For å få den nåværende datoen i Bash, kan du bruke kommandoen "date" sammen med ønsket format. For eksempel, for å få datoen i standard format, kan du skrive følgende i terminalen:

```Bash
date
```

Dette vil gi output som dette:

```Bash 
Fre Aug 13 19:16:53 CEST 2021
```

Du kan også formatere utdataen ved å inkludere spesifikke flagg. For eksempel, for å få datoen i formatet "DD/MM/YYYY", skriver du:

```Bash
date +'%d/%m/%Y'
```

Dette vil gi følgende output:

```Bash
13/08/2021
```

Det finnes en rekke ulike formateringsflagg du kan bruke for å få ønsket output. En fullstendig liste finner du [her](https://www.gnu.org/software/coreutils/manual/html_node/Examples-using-date.html#Examples-using-date).

## Dypdykk

Date-kommandoen henter faktisk ikke bare den nåværende datoen, men også klokkeslett og tidssone. Du kan også bruke den til å stille inn en fremtidig eller tidligere dato. For eksempel, for å få datoen 7 dager frem i tid, skriver du:

```Bash
date -d '+7 days'
```

Dette vil gi en output som dette:

```Bash
Lør 21 Aug 19:33:30 CEST 2021
```

I tillegg til å vise den nåværende datoen, kan datokommandoen også brukes til å endre systemets dato og klokkeslett. Dette kan være nyttig hvis du for eksempel skal teste et program på en bestemt dato eller tid.

## Se Også

- [GNU Coreutils - date](https://www.gnu.org/software/coreutils/manual/html_node/date-invocation.html)
- [Linuxize - How To Get Current Date and Time in Bash](https://linuxize.com/post/bash-current-date-and-time/)
- [TecMint - Basic Date and Time Management in Linux using 'date' Command](https://www.tecmint.com/date-command-examples/)