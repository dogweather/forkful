---
title:    "Fish Shell: Sammenligner to datoer."
keywords: ["Fish Shell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/no/fish-shell/comparing-two-dates.md"
---

{{< edit_this_page >}}

# Hvorfor

Å sammenligne to datoer kan være nyttig for å organisere og analysere data. Dette kan hjelpe deg med å finne ut hvilke datoer som er nærmest eller lengst fra hverandre, og å se trender over tid.

# Hvordan

Du kan enkelt sammenligne to datoer ved hjelp av Fish Shell. Her er noen eksempler på hvordan du kan gjøre det:

```
# Setter to datoer som variabler

set startdato 2021-01-01
set sluttdato 2021-06-01

# Sammenligner datoene ved hjelp av kommandoen 'test'

if test $startdato -ge $sluttdato
    echo "Startdatoen er større eller lik sluttdatoen"
else if test $startdato -lt $sluttdato
    echo "Startdatoen er mindre enn sluttdatoen"
end

# Output: Startdatoen er mindre enn sluttdatoen
```

Du kan også formatere datoer på forskjellige måter for å få mer nøyaktige sammenligninger. For eksempel kan du bruke kommandoen `date` for å vise datoen på en bestemt måte:

```
set startdato (date -f %Y-%m-%d -d "1 januar 2021")
set sluttdato (date -f %Y-%m-%d -d "1 juni 2021")

# Output: 2021-01-01
```

Merk at disse eksemplene bruker standard norsk datoformat (år-måned-dag). Du kan justere formatet til ditt språks standard.

# Dypdykk

Fish Shell har også innebygde funksjoner for å arbeide med datoer. Du kan bruke `math`-kommandoen til å utføre matematiske operasjoner på datoer. For eksempel kan du legge til eller trekke fra et antall dager fra en dato:

```
set startdato (date -d "1 januar 2021")
set sluttdato (math $startdato + 5 days)
```

Dette vil gi deg sluttdatoen 5 dager etter startdatoen.

En annen nyttig funksjon er `string`, som kan konvertere en dato til en streng. Dette kan være nyttig når du jobber med forskjellige format:

```
set startdato (date -d "1 januar 2021")
set startdatostring (string "%d/%m/%Y" $startdato)

# Output: 01/01/2021
```

# Se også

Her er noen flere ressurser som kan være nyttige når du jobber med datoer i Fish Shell:

- [Fish Shell dokumentasjon om datoer](https://fishshell.com/docs/current/cmds/date.html)
- [En veiledning til datoformatet i Unix](https://www.computerhope.com/unix/timestamp.htm)
- [En sammenligning av forskjellige programmeringsspråks funksjoner for datoer](https://codereview.stackexchange.com/questions/173581/comparing-two-dates-in-different-programming-languages)