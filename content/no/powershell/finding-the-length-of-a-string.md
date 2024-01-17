---
title:                "Å finne lengden til en streng"
html_title:           "PowerShell: Å finne lengden til en streng"
simple_title:         "Å finne lengden til en streng"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/powershell/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Finn lengden til en tekststreng er en måte å bestemme antall tegn i en tekst på, inkludert mellomrom og spesialtegn. Dette er nyttig for å behandle eller formatere tekst, og er en vanlig oppgave for utviklere.

## Hvordan:
### PowerShell eksempel:
```
$tekststreng = "Hei, verden!"
$tekststreng.Lengde
```
### Resultat:
```
12
```

## Deep Dive:
Når datamaskinen lagrer tekststrenger, gir den hvert tegn en numerisk verdi kalt en "kodepoeng". Å finne lengden til en tekststreng innebærer å telle antall kodepoeng.

En alternativ måte å finne lengden på en tekststreng i PowerShell er å bruke cmdlet-en `Measure-Object`. Denne kan også brukes til å finne lengden til en CSV-fil eller et objekt.

Implementasjonsdetaljer: I PowerShell blir tekststrenger behandlet som .NET `System.String` objekter. Dette betyr at `.Lengde` egenskapen er tilgjengelig for å finne lengden til teksten.

## Se Også:
- [PowerShell .Lengde egenskapen](https://docs.microsoft.com/nb-no/powershell/module/microsoft.powershell.core/about/about_string_objects?view=powershell-7#properties-of-a-string-object)
- [Measure-Object cmdlet-en](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.utility/measure-object?view=powershell-7)