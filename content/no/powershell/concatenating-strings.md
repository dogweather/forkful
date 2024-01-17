---
title:                "Sammenslåing av strenger"
html_title:           "PowerShell: Sammenslåing av strenger"
simple_title:         "Sammenslåing av strenger"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/powershell/concatenating-strings.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Å legge sammen strenger er en vanlig oppgave i programmering, der man kombinerer flere tekststrenger for å danne en enkelt streng. Dette er nyttig for å lage dynamiske meldinger og tilpasse utdataen basert på variabler.

## Hvordan gjør man det:
For å legge sammen strenger i PowerShell, bruker man operatoren `+` som viser string-concatenation-funksjonalitet. Se eksemplet nedenfor:
```
PowerShell $navn = "Anna"
$xoxo = " hugs and kisses!"
Write-Output "Hei " + $navn + "!" + $xoxo
```
Eksempelutskrift:
```
Hei Anna! xoxo hugs and kisses!
```

## Dykk dypere:
Strengkonkateneringsbegrepet har sin opprinnelse fra programmeringspråket Basic. I Powershell er det også mulig å bruke metoder som `Concat()` og `Join()` for å legge sammen strenger. I tillegg finnes det alternative metoder som `StringBuilder`-klassen og `Format()`-metoden. Ved å bruke disse alternative metodene kan man forbedre ytelsen ved å redusere antall allokeringsoperasjoner.

## Se også:
For mer informasjon om strengkonkatenasjon i PowerShell, sjekk ut følgende kilder: 
- [Microsoft Docs om string concatenation](https://docs.microsoft.com/en-us/powershell/scripting/developer/cmdlet/converting-a-number-to-a-string?view=powershell-7)