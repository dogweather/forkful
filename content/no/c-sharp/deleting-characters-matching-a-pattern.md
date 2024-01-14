---
title:                "C#: Slette tegn som matcher et mønster"
simple_title:         "Slette tegn som matcher et mønster"
programming_language: "C#"
category:             "C#"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/c-sharp/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Hvorfor

I noen tilfeller kan det være nødvendig å fjerne karakterer som samsvarer med et visst mønster fra en tekststreng. Dette kan være nyttig for å rense data eller for å filtrere ut uønsket informasjon.

## Hvordan

Det første vi må gjøre er å importere "System.Text.RegularExpressions" biblioteket for å kunne bruke regex-funksjoner i koden vår:

```C#
using System.Text.RegularExpressions;
```

Deretter definerer vi teksten vi ønsker å behandle, for eksempel en tekststreng som inneholder flere e-postadresser:

```C#
string tekst = "Se på disse e-postadressene: test@test.com, test2@test.com, test3@test.com"
```

For å fjerne alle e-postadresser fra teksten, kan vi bruke Regex.Replace() metoden og spesifisere et regex-mønster og hva det skal erstattes med. I dette tilfellet vil vi erstatte alle e-postadresser med en tom streng, som vil resultere i at de blir fjernet helt:

```C#
string renTekst = Regex.Replace(tekst, @"[\w\.\-]+@[\w\.\-]+\.\w+", "");
```

Den resulterende teksten vil nå være: "Se på disse e-postadressene: , , ".

## Dypdykk

Regex, eller regulære uttrykk, er et kraftig verktøy for å søke og manipulere tekst. Det lar oss definere et mønster som skal matches mot en tekststreng, og deretter utføre ulike operasjoner basert på dette mønsteret. Ved å bruke regex, kan vi oppnå mye mer kompleks og presis behandling av tekst enn ved å bruke vanlige tekstmanipuleringsfunksjoner.

Når vi bruker Regex.Replace() metoden, må vi spesifisere et regex-mønster som angir hvordan deler av teksten vi ønsker å erstatte, skal se ut. I eksempelet ovenfor har vi brukt et mønster som matcher alle e-postadresser basert på et gitt format. Dette mønsteret kan endres og tilpasses ulike formater og typer informasjon man ønsker å fjerne.

## Se også

- [Microsoft dokumentasjon om regulære uttrykk i C#](https://docs.microsoft.com/en-us/dotnet/standard/base-types/regular-expressions)
- [Tutorial om regulære uttrykk i C#](https://www.c-sharpcorner.com/learn/regex-in-C-Sharp/)