---
title:                "Interpolering av streng"
html_title:           "PowerShell: Interpolering av streng"
simple_title:         "Interpolering av streng"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/powershell/interpolating-a-string.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?

Strenginterpolasjon er en viktig del av programmering i PowerShell. Det er en måte å sette sammen en streng ved å flette inn variabler eller andre uttrykk på en effektiv måte. Dette gjør det enkelt og praktisk å manipulere og formatere tekst i skript.

## Slik gjør du:

Be om brukerens navn og ønskede alder ved hjelp av Read-Host cmdlet.
PowerShell kodeblokker:
```PowerShell
$navn = Read-Host "Hva er navnet ditt?"
$alder = Read-Host "Hvor gammel er du?"
Write-Host "Hei $navn, du er $alder år gammel!"
```
Output:
```
Hva er navnet ditt? John
Hvor gammel er du? 25
Hei John, du er 25 år gammel!
```

## Ned i detalj:

Strenginterpolasjon ble introdusert i PowerShell 2.0 for å gjøre det enklere å håndtere tekst og variabler i skript. Tidligere var man avhengig av å bruke string concatenation, som innebar mye mer kode og potensielt flere feil. Det finnes også alternative metoder for å interpolere en streng i PowerShell, som for eksempel ved å bruke den innebygde "format" -metoden.

Strenginterpolasjonen er implementert ved å plassere variabler eller uttrykk innenfor et settefjes: ```$variabel```. Når PowerShell kjører koden, vil den erstatte settefjeset med verdien til variabelen eller uttrykket. 

## Se også:

Offisiell dokumentasjon for strenginterpolasjon i PowerShell - https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.core/about/about_operators 

En godt skrevet guide til strenginterpolasjon i PowerShell - https://www.red-gate.com/simple-talk/dotnet/net-development/powershell-stringing-us-little-bit/