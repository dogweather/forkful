---
title:                "Elm: Uttrekking av understrenger"
simple_title:         "Uttrekking av understrenger"
programming_language: "Elm"
category:             "Elm"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/elm/extracting-substrings.md"
---

{{< edit_this_page >}}

## Hvorfor

Hvorfor skulle noen ønske å eksktrahere substringer i Elm-programmering? Vel, det er flere grunner til dette. En av de viktigste grunnene er at det kan gjøre koden din mer effektiv og lesbar. Ved å eksktrahere substringer kan du få tilgang til spesifikke deler av tekst uten å måtte håndtere hele strengen. Dette gjør det enklere å manipulere og bruke data på en mer presis måte.

## Slik gjør du det

For å eksktrahere substringer i Elm, kan du bruke funksjonen `String.slice start end text`. Dette vil returnere en del av `text` fra `start`-indeksen til `end`-indeksen. La oss se på et enkelt eksempel:

```Elm
import String

text = "Hei alle sammen"
substring = String.slice 4 7 text
```

I dette eksempelet vil `substring` ha verdien "alle". `start`-indeksen er alltid inkludert, mens `end`-indeksen er ekskludert. Dette betyr at i dette tilfellet, vil den returnerte delen starte på indeksen 4 og gå til, men ikke inkludere, indeksen 7.

Hvis du vil eksktrahere den første delen av en streng, kan du også bruke `String.left`-funksjonen. For å få den siste delen av en streng, kan du bruke `String.right`-funksjonen. La oss se på et eksempel på begge disse funksjonene:

```Elm
import String

text = "Markus"
firstName = String.left 3 text
lastName = String.right 3 text
```

Her vil `firstName` ha verdien "Mar" og `lastName` vil ha verdien "kus". Begge funksjonene tar inn et heltall som representerer antall tegn som skal hentes fra begynnelsen eller slutten av strengen.

## Dykk dypere

Det er også mulig å eksktrahere substringer basert på et bestemt tegn eller et sett med tegn. For å gjøre dette, kan du bruke `String.split`-funksjonen som returnerer en liste med substringer delt opp ved hjelp av det spesifikke tegnet. La oss se på et eksempel:

```Elm
import String

text = "apple,banana,orange"
fruits = String.split "," text
```

Her vil `fruits`-listen ha tre verdier: "apple", "banana" og "orange". `String.split`-funksjonen kan også ta inn et sett av tegn som separasjonspunkt. For eksempel kan du splitte en streng basert på mellomrom ved å bruke `String.split [" "] tekst` som vil returnere en liste med ordene i teksten.

## Se Også

- [Offisiell Elm Dokumentasjon om Strenger](https://guide.elm-lang.org/strings/)
- [Elm String Bibliotek](https://package.elm-lang.org/packages/elm/core/latest/String)