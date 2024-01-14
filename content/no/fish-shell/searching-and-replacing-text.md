---
title:                "Fish Shell: Søking og erstatt av tekst"
programming_language: "Fish Shell"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/fish-shell/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

# Hvorfor

Har du noen gang sittet og endret tekst i en lang fil manuelt, og følt at det tar evig lang tid? Det er her Fish Shell kommer inn i bildet. Med Fish Shell's innebygde funksjoner for å søke og erstatte tekst, kan du effektivt og raskt automatisere denne oppgaven.

# Slik gjør du det

Å søke og erstatte tekst er en enkel prosess i Fish Shell. Du trenger bare å bruke `sed` kommandoen, og spesifisere teksten du vil søke etter og erstatte med. For eksempel, la oss si at du vil endre alle forekomster av "hello" til "hei" i en fil med navnet "tekstfil.txt". Her er et eksempel på hvordan du kan gjøre det:

```
Fish Shell sed -i 's/hello/hei/g' tekstfil.txt
```

I dette eksempelet bruker vi kommandoen `sed` med flagget `-i` for å erstatte tekst direkte i filen. Vi bruker også regex uttrykket `s/hello/hei/g` for å spesifisere at vi vil erstatte alle forekomster av "hello" med "hei". Enkelt og effektivt!

# Dypdykk

Det er mange forskjellige måter å bruke `sed` kommandoen på for å søke og erstatte tekst, og Fish Shell tilbyr også en rekke andre nyttige funksjoner for dette. Du kan for eksempel bruke wildcard karakterer for å søke etter tekst som inneholder et bestemt mønster, og deretter erstatte den delen av teksten med en annen tekst. Du kan også kombinere `sed` kommandoen med andre Fish Shell kommandoer for å opprette mer avanserte søke- og erstatningsprosesser.

# Se også

For mer informasjon og eksempler på hvordan du kan søke og erstatte tekst i Fish Shell, se følgende ressurser:

- [Fish Shell dokumentasjon](https://fishshell.com/docs/current/cmds/sed.html)
- [Effektive Shell-skripting Tips](https://dev.to/d3v3sh5/software-automation-with-fishing-shell-script-automation-series-4763) (på engelsk)