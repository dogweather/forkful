---
title:                "Konvertere en streng til små bokstaver"
html_title:           "PowerShell: Konvertere en streng til små bokstaver"
simple_title:         "Konvertere en streng til små bokstaver"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/powershell/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Når vi konverterer en streng til små bokstaver, endrer vi alle bokstaver til små bokstaver. Dette kan være nyttig for å sammenligne tekst eller for å sikre konsistent formatering. Programmere gjør dette for å behandle tekst ensartet uavhengig av hvordan den er skrevet.

## Hvordan:
I PowerShell kan du enkelt konvertere en streng til små bokstaver ved hjelp av kommandoen ```ToLower()```. Se eksempelet nedenfor for å se hvordan dette fungerer.

```PowerShell
$tekst = "Det er en tekst"
$tekst.ToLower()
```
**Output:**
```
det er en tekst
```

## Dykk dypt:
Konvertering av strenger til små bokstaver har vært et vanlig prinsipp i programmering siden tidlig på 1970-tallet. Alternativet til å bruke ```ToLower()``` i PowerShell er å bruke en grensesnitt-funksjon i .NET-rammeverket. Dette kan være litt mer krevende, men gir også større fleksibilitet for avansert tekstbehandling. I PowerShell er det også mulig å bruke ```-LowerCase``` parameteren på kommandoene ```Where-Object``` og ```Sort-Object``` for å manipulere tekst.

## Se også:
- [Microsoft Docs: ToLower()](https://docs.microsoft.com/nb-no/dotnet/api/system.string.tolower?view=netframework-4.8)
- [PowerShell Guru: Strings](https://powershell.org/tag/strings/)
- [SS64: Lowercase](https://ss64.com/ps/syntax-lowercase.html)