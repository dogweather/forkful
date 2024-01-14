---
title:                "Fish Shell: Søking og erstattning av tekst"
simple_title:         "Søking og erstattning av tekst"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/fish-shell/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Hvorfor

Når man jobber med tekstbehandling, enten det er å skrive koder eller å redigere dokumenter, er det ofte behov for å gjøre store endringer på tekst. Kanskje må man endre en bestemt del av teksten på flere steder, eller kanskje må man endre ett ord til et annet flere ganger. Dette kan gjøres manuelt, men det kan også være en veldig tidkrevende og feilbar prosess. Her kommer Fish Shell og dens søke- og erstatningsfunksjonalitet inn i bildet.

## Hvordan

For å søke og erstatte tekst i Fish Shell, kan man bruke kommandoen "sed". Ved hjelp av denne kommandoen kan man søke etter bestemte tekststrenger og erstatte dem med ønsket tekst. La oss for eksempel si at vi ønsker å endre alle forekomster av ordet "hund" til "katt" i en tekstfil ved navn "dyrenavn.txt":

```
sed 's/hund/katt/g' dyrenavn.txt
```

Her står "s" for "substitute" (erstatt), "hund" er ordet vi ønsker å erstatte, "katt" er ordet vi ønsker å erstatte med, og "g" står for "global" (altså alle forekomster). Etter denne kommandoen vil alle forekomster av "hund" i teksten ha blitt erstattet med "katt".

## Dypdykk

Det fins også flere alternativer og variabler man kan bruke i "sed"-kommandoen for mer nøyaktig søk og erstatning. For eksempel kan man bruke "-i" for å gjøre endringene direkte i filen, "-n" for å kun vise endringene uten å faktisk gjøre dem, og "-r" for å bruke regulære uttrykk i stedet for enkle søkestrenger.

## Se også

- Her kan du lære mer om søke- og erstatningsfunksjonaliteten til Fish Shell: https://fishshell.com/docs/current/cmds/sed.html
- Hvis du ønsker å lære mer om Fish Shell generelt: https://fishshell.com/docs/current/index.html
- En norsk ressurs for å lære mer om kommandolinjen og tekstbehandling i Linux: https://manual.gnome.org/stable//gnome-help/shell-introduction.html.no