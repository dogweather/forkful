---
title:                "Søke og erstatte tekst"
html_title:           "PowerShell: Søke og erstatte tekst"
simple_title:         "Søke og erstatte tekst"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/powershell/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Søking og erstatting av tekst handler om å endre eller erstatte deler av en tekststreng. Dette er en vanlig oppgave for programmører når de ønsker å endre mange forekomster av en bestemt tekst.

## Slik gjør du det:
Å søke og erstatte tekst i PowerShell er enkelt og kan gjøres ved hjelp av innebygde funksjoner og operatorer. Se eksemplene nedenfor for å lære hvordan det gjøres.

### Bytt ut en enkelt forekomst:
```PowerShell
"Hello world".Replace("world","universe")
```
Output: Hello universe

### Bytt ut flere forekomster:
```PowerShell
"Jeg liker å spise is hver dag".Replace("e"," ")
```
Output: Jeg lik r å spis is hv år dag

## Dype dykk:
Søking og erstatting av tekst er en nødvendig del av programmering og har vært en vanlig oppgave siden de tidligste dager av datamaskiner. Det finnes også flere alternative måter å gjøre dette på, for eksempel ved hjelp av regex eller diverse tekstredigeringsprogrammer. I PowerShell er tekststrenger merket med anførselstegn eller enkle anførselstegn, og operatøren `.Replace()` brukes til å utføre søking og erstatting.

## Se også:
- [PowerShell - Replace Method](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.utility/replace-method?view=powershell-7.1)
- [Working with Strings in PowerShell](https://www.petri.com/working-with-strings-in-powershell)