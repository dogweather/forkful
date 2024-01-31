---
title:                "Interpolering av en streng"
date:                  2024-01-20T17:51:40.752747-07:00
model:                 gpt-4-1106-preview
simple_title:         "Interpolering av en streng"

category:             "PowerShell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/powershell/interpolating-a-string.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?

Interpolering av strenger er rett og slett å putte variabler eller uttrykk inni en tekststreng. Programmerere bruker det for å bygge dynamiske meldinger eller kommandoer uten å måtte slite med å legge sammen mange små tekstbiter.

## Hvordan:

```PowerShell
$name = 'Viking'
$age = 32

# Interpolere med dobbelt anførselstegn
$message = "Hei, jeg heter $name og jeg er $age år gammel."
Write-Output $message

# Output: Hei, jeg heter Viking og jeg er 32 år gammel.

# Interpolere med subexpression "${}"
$greeting = "Hei, ${name}en!"
Write-Output $greeting

# Output: Hei, Vikingen!
```

## Dypdykk

Stringinterpolering, også kjent som variabelsubstitusjon, har vært en del av programmeringsspråk lenge. I PowerShell bruker vi `"` (dobbel anførselstegn) for å oppnå dette. Historisk sett var vi nødt til å bruke plussoperatoren (+) for å legge sammen strenger og variabler, som kunne bli ganske rotete og vanskelig å lese.

Alternativer til interpolering i PowerShell inkluderer bruk av `-f` operatøren (format operator) eller funksjonen `String.Format()`, som begge tillater mer detaljert formatering av strenger.

Interpolering forenkler skriving av koden og gjør den mer lesbar. Det gjør at koden automatisk konverterer variabelverdier til tekststrengen, uten ekstra arbeid fra programmereren.

Det er viktig å huske at i PowerShell fungerer kun stringinterpolering med dobbelt anførselstegn. Enkelt anførselstegn blir tolket som en literal streng, og eventuelle variabler blir ikke evaluert.

## Se Også

- [PowerShell Dokumentasjon](https://docs.microsoft.com/powershell/)
- [Digital kurs i PowerShell på Microsoft Learn](https://docs.microsoft.com/learn/paths/powershell/)
