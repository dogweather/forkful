---
title:    "Bash: Sammenligning av to datoer"
keywords: ["Bash"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/no/bash/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Hvorfor
Å sammenligne to datoer kan være en nyttig ferdighet i Bash-programmering. Det kan være nyttig i situasjoner der du trenger å sjekke om en dato kommer før eller etter en annen, eller om de er like.

## Hvordan
For å sammenligne to datoer i Bash, kan du bruke `date` og `test` kommandoene. Her er et eksempel på hvordan du kan sammenligne to datoer for å se om den første er før den andre:

```
#!/bin/bash

# Lag to variabler med datoer
date1="2021-01-01"
date2="2021-02-01"

# Sjekk om date1 er før date2
if [[ $(date -d "$date1" +%Y%m%d) -lt $(date -d "$date2" +%Y%m%d) ]]
then
  echo "$date1 er før $date2"
fi
```

Output for dette eksemplet vil være:
```
2021-01-01 er før 2021-02-01
```

Du kan også bruke `test` kommandoen for å sjekke om to datoer er like eller ikke:

```
#!/bin/bash

# Lag to variabler med datoer
date1="2021-01-01"
date2="2021-01-01"

# Sjekk om date1 er lik date2
if [[ $(date -d "$date1" +%Y%m%d) == $(date -d "$date2" +%Y%m%d) ]]
then
  echo "$date1 er lik $date2"
fi
```

Output vil være:
```
2021-01-01 er lik 2021-01-01
```

## Deep Dive
Når vi sammenligner to datoer i Bash, konverterer vi dem først til et tall ved hjelp av `date` kommandoen. Dette gjør det lettere å sammenligne datoene ved å bruke de vanlige matematiske sammenligningsoperatorene som `>` (større enn) og `<` (mindre enn). Ved å bruke `+%Y%m%d` formatet, får vi ut datoene som tall i åtte siffer, som gjør det enklere å sammenligne dem.

Husk også at formatet for datoer kan variere i forskjellige land og systemer, derfor er det viktig å sjekke hva som er det riktige formatet på datoene du sammenligner.

## Se Også
- [Bash-dokumentasjonen om `date` og `test` kommandoene](https://www.gnu.org/software/bash/manual/html_node/Shell-Arithmetic.html)
- [En guide til å konvertere datoer til tall i Bash](https://unix.stackexchange.com/questions/25025/how-to-compare-two-dates-in-a-shell)