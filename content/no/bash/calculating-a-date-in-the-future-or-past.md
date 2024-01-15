---
title:                "Beregning av en dato i fremtiden eller fortiden"
html_title:           "Bash: Beregning av en dato i fremtiden eller fortiden"
simple_title:         "Beregning av en dato i fremtiden eller fortiden"
programming_language: "Bash"
category:             "Bash"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/bash/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Hvorfor
Det kan være mange grunner til å ville beregne en dato i fremtiden eller fortiden. Kanskje du ønsker å planlegge en begivenhet eller sjekke når en gjeldende dato vil falle på en bestemt ukedag. Uansett årsak, å kunne beregne datoer i Bash kan være en svært nyttig ferdighet å ha.

## Slik Gjør Du Det
Det er flere måter å beregne datoer i Bash, men en enkel måte er å bruke `date` kommandoen. For å beregne en dato i fremtiden, bruk følgende syntaks:

```Bash
future_date=$(date -d "1 week" +%Y-%m-%d)
echo $future_date 
```

Her vil `future_date` variabelen beregne datoen for en uke frem i tid og lagre den i variabelen. Deretter vil `echo` kommandoen skrive ut datoen i formatet YYYY-MM-DD. Du kan endre "1 week" til å være for eksempel "2 days" eller "1 year" for å beregne en annen dato.

For å beregne en dato i fortiden, bruk denne syntaksen:

```Bash
past_date=$(date -d "2 weeks ago" +%d/%m/%y)
echo $past_date
```

Her vil `past_date` variabelen beregne datoen for to uker siden og lagre den i variabelen. Deretter vil `echo` kommandoen skrive ut datoen i formatet DD/MM/YY. Du kan endre "2 weeks ago" til å være for eksempel "3 months ago" eller "-1 year" for å beregne en annen dato.

## Dykk Dypere
For de som er interessert i å forstå hvordan denne kommandoen fungerer, bruker den `-d` flagget for å angi en dato og tiden som skal beregnes fra. Dette kan være i form av en bestemt dato, som "December 25", eller en tidsperiode, som "1 week ago". Deretter brukes `%` tegnene til å spesifisere formatet på datoen som ønskes utskrevet.

For en full liste over de ulike formatene som kan brukes med `date` kommandoen, kan du se Bash `man` siden (`man date`) eller sjekk ut denne [artikkelen](https://www.computerhope.com/unix/date.htm).

## Se Også
- [Bash `date` Kommando](https://www.computerhope.com/unix/date.htm)
- [`man` side for `date` kommandoen](man date)