---
title:                "Fish Shell: Sammenligning av to datoer"
simple_title:         "Sammenligning av to datoer"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/fish-shell/comparing-two-dates.md"
---

{{< edit_this_page >}}

# Hvorfor
I denne bloggposten skal vi se nærmere på hvordan man kan sammenligne to datoer ved hjelp av Fish Shell programmeringsspråk. Dette kan være nyttig når man ønsker å filtrere eller sortere data etter dato.

# Hvordan gjøre det
For å sammenligne to datoer i Fish Shell, kan man bruke kommandoen `date1 > date2` der `date1` og `date2` er de to datoene som skal sammenlignes. Her er et eksempel på hvordan dette kan gjøres:

```Fish Shell
set start_date (date -u +'%Y-%m-%d')
set end_date (date -u +'%Y-%m-%d' -d '2 days ago')

if test $start_date \> $end_date
    echo "Start dato er senere enn sluttdato"
else if test $start_date == $end_date
    echo "Start og sluttdato er lik"
else
    echo "Start dato er tidligere enn sluttdato"
end
```

I dette eksempelet brukes `date`-kommandoen til å sette variablene `start_date` og `end_date` til dagens dato og dagen før. Deretter brukes `test`-kommandoen til å sammenligne datoene og gi en tilbakemelding basert på resultatet. Resultatet av dette eksempelet vil være `Start dato er tidligere enn sluttdato` ettersom `start_date` er satt til dagens dato mens `end_date` er satt til dagen før.

# Dypdykk
Det finnes ulike måter å sammenligne datoer på i Fish Shell, blant annet ved å bruke operatørene `>`, `<`, `==` og `!=` i kombinasjon med `date`-kommandoen. Man kan også bruke `test`-kommandoen med argumentet `-nt` for å sammenligne om en fil er nyere enn en annen. Dette kan være nyttig i situasjoner der man ønsker å sjekke om en fil har blitt endret etter en bestemt dato.

# Se også
- [Fish Shell dokumentasjon](https://fishshell.com/docs/current/index.html)
- [Bruk av dato i Fish Shell](https://fishshell.com/docs/current/tutorial.html#use-dates)
- [Sammenligning av data i Fish Shell](https://fishshell.com/docs/current/tutorial.html#conditionals)