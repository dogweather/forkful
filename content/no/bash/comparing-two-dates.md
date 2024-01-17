---
title:                "Sammenligning av to datoer"
html_title:           "Bash: Sammenligning av to datoer"
simple_title:         "Sammenligning av to datoer"
programming_language: "Bash"
category:             "Bash"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/bash/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Hva og hvorfor?
Sammenligning av to datoer er en vanlig oppgave for programmerere. Det er en måte å sjekke om en dato er større, mindre eller lik en annen dato. Det er nyttig for å organisere og filtrere data basert på datoer.

## Slik gjør du:
Bruk følgende syntaks for å sammenligne to datoer i Bash:
```
if [ "$date1" -eq "$date2" ]; then
  echo "Datoene er like"
elif [ "$date1" -lt "$date2" ]; then
  echo "Dato1 er før Dato2"
else
  echo "Dato1 er etter Dato2"
fi
```

Eksempelutdata:
```
date1=2021-01-01
date2=2021-01-15
Dato1 er før Dato2
```

## Dykk dypere:
I eldre versjoner av Bash ble datofunksjonen "test" brukt for å sammenligne datoer. Men med den nyere versjonen av Bash, kan du bruke operatøren "-eq" for numeriske sammenligninger. Alternativt kan du også bruke Unix-verktøyet "date" for å konvertere datoer til epokenummer og sammenligne dem. Implementeringen av sammenligningen er basert på ISO-datoformatet, og kan variere mellom forskjellige operativsystemer.

## Se også:
- [Bash Manual](https://www.gnu.org/software/bash/manual/html_node/Bash-Conditional-Expressions.html)
- [Using Date in Bash Scripts](https://www.baeldung.com/linux/bash-date-command)