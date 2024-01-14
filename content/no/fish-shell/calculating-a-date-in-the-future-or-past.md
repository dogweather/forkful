---
title:                "Fish Shell: Beregning av en dato i fremtiden eller fortiden"
programming_language: "Fish Shell"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/fish-shell/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Hvorfor
Det kan være mange grunner til å beregne en dato i fremtiden eller fortiden, som for eksempel å planlegge reiser eller viktige hendelser, eller for å håndtere forfallsdatoer for regninger eller avtaler.

## Hvordan bruke Fish Shell til å beregne datoer
For å beregne en dato i fremtiden, kan du bruke følgende kode i Fish Shell:

```Fish Shell
set -l dato (date -d "1 week" +%Y-%m-%d)
echo $dato
```

Dette vil gi deg datoen i dag plus en uke. Du kan også erstatte "1 week" med en annen tidsperiode, som for eksempel "3 months" eller "1 year".

For å beregne en dato i fortiden, må du erstatte "1 week" med "1 week ago". For eksempel:

```Fish Shell
set -l dato (date -d "1 week ago" +%Y-%m-%d)
echo $dato
```

Dette vil gi deg datoen for en uke siden.

## Dypdykk
Fish Shell gir deg muligheten til å bruke ulike parameter for å beregne datoer i fremtiden eller fortiden. Noen nyttige eksempler på dette er "-d" for å definere dato og "-f" for å formatere utdataen.

Det er også mulig å kombinere flere parameter for å få en mer spesifikk dato. For eksempel:

```Fish Shell
set -l dato (date -d "1 week 1 day" -f "%d. %B %Y")
echo $dato
```

Dette vil gi deg datoen for en uke og en dag frem i tid, i formatet for eksempel "26. august 2020".

## Se også
- [Fish Shell dokumentasjon](https://fishshell.com/docs/current/cmds/date.html)
- [Guide til å formatere datoer med Fish Shell](https://www.baeldung.com/linux/fish-format-date)
- [Video tutorial om å beregne datoer med Fish Shell](https://www.youtube.com/watch?v=dQw4w9WgXcQ)