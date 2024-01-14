---
title:                "Fish Shell: Sletting av tegn som matcher et mønster"
programming_language: "Fish Shell"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/fish-shell/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Hvorfor

Noen ganger kan det være nyttig å kunne slette visse tegn eller mønstre fra en tekststreng. Dette kan være nyttig når du for eksempel ønsker å fjerne ekstra mellomrom, spesialtegn eller et bestemt ord fra en tekst.

## Slik gjør du det

For å kunne slette tegn etter et spesifikt mønster i Fish Shell, kan du bruke kommandoen `sed`. Dette står for stream editor og kan brukes til å manipulere tekststrenger på en effektiv måte.

En enkel måte å bruke `sed` på er å kombinere den med kommandoen `tr`, som står for translate. Dette lar deg erstatte tegnene som matcher mønsteret med et annet tegn, eller slette dem helt.

```Fish Shell
echo "Dette er en tekststreng" | sed 's/en//g' | tr -d " "
```

Dette eksempelet vil fjerne alle forekomster av "en" og slette alle mellomrom i teksten. Resultatet vil bli "Dtteistrk".

En annen måte å slette tegn eller mønstre i en tekst på er å bruke kommandoen `grep`. Dette vil filtrere ut linjer som inneholder mønsteret du ønsker å slette, og deretter bruke kommandoen `tr` for å fjerne tegnene.

```Fish Shell
grep -v "tekst" < filnavn | tr -d " "
```

Her vil kommandoen først filtrere ut alle linjer som inneholder ordet "tekst" og deretter fjerne alle mellomrom fra resultatet.

## Mer avansert

Dette er bare to enkle måter å slette tegn eller mønstre i Fish Shell, men det finnes flere muligheter og kombinasjoner du kan bruke for å oppnå ønsket resultat. For å lære mer om andre kommandoer og funksjoner du kan bruke, kan du lese dokumentasjonen for Fish Shell og prøve deg frem med forskjellige kombinasjoner.

## Se også

- Fish Shell dokumentasjon (https://fishshell.com/docs/current/index.html)
- Sed og tr kommandoer (https://www.gnu.org/software/sed/manual/sed.html, https://www.gnu.org/software/coreutils/manual/html_node/tr-invocation.html)