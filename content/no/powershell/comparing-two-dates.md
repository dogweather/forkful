---
title:                "Sammenligner to datoer"
html_title:           "Clojure: Sammenligner to datoer"
simple_title:         "Sammenligner to datoer"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/powershell/comparing-two-dates.md"
---

{{< edit_this_page >}}

# PowerShells Datostammening: En Kortfattet Guide

## Hva & Hvorfor?

Å sammenligne to datoer er prosessen med å se hvilken dato som er tidligste eller seneste, eller om de er like. Programmerere gjør dette for å sortere hendelser, bestemme tidsperioder, og tilpasse systematisk behandling av data.

## Hvordan?

La oss sammenligne to datoer i PowerShell ved å bruke `-gt` (greater than = større enn), `-lt` (less than = mindre enn), og `-eq` (equals = lik).

```PowerShell
# Definer to datoer
$dato1 = Get-Date -Year 2022 -Month 2 -Day 1
$dato2 = Get-Date -Year 2022 -Month 2 -Day 2

# Sammenlign datoene
$dato1 -lt $dato2    # Returnerer True
$dato1 -gt $dato2    # Returnerer False
$dato1 -eq $dato2    # Returnerer False
```

Resultatet vil være `True` dersom det første vilkåret er oppfylt, og `False` dersom det ikke er det.

## Deep Dive

Sammenligning av datoer er nøkkelen til mange programmeringsoppgaver i PowerShell. Historisk, å dra håndteringen av dato og tid i programmering har vært utfordrende grunnet forskjellige tidssoner, skuddår og lokale forskjeller, men PowerShell har gjort dette enklere.

Selv om metoden ovenfor er den mest anvendelige, finnes det alternativer. Tidspunktene kan trekkes fra hverandre for å skape en TimeSpan-objekt, som så kan analyseres. Skriptene kan også konvertere datoene til strenger og sammenligne alfabetisk, selv om dette krever forsiktig formatering.

Når du sammenligner datoer i PowerShell, er det viktig å merke at tiden på dagen også sammenlignes. Dersom du kun vil sammenligne datoene, pass på at tidsdelen er lik for begge datoene.

## Se Også

* Microsofts Dokumentasjon om dato/tid i PowerShell: https://docs.microsoft.com/en-us/powershell/scripting/samples/working-with-dates-and-times?view=powershell-7.1
* En detaljert guide om hvordan å bruke "-lt", "-gt", og "-eq" i PowerShell: https://www.tutorialspoint.com/powershell/powershell_operators.htm
* En omfattende liste over PowerShell-kommandoer: https://ss64.com/ps/