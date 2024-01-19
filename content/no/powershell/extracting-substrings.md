---
title:                "Utdrag av understrenger"
html_title:           "Bash: Utdrag av understrenger"
simple_title:         "Utdrag av understrenger"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/powershell/extracting-substrings.md"
---

{{< edit_this_page >}}

---

# Utdrag av delstrenger i PowerShell

---

## Hva & Hvorfor?

I programmering, er utdrag av delstrenger prosessen med å ta en del av en streng og gjøre den om til en ny streng. Vi gjør dette i flere tilfellene, for eksempel for å analysere loggfiler, sanitere input data eller manipulere tekst.

---

## Hvordan:

### Eksempel 1:
Du kan trekke ut en delstreng fra en streng ved å bruke metoden `.Substring()` i PowerShell. For eksempel:
``` PowerShell
$streng = "Hallo Verden"
$delstreng = $streng.Substring(6)
Write-Output $delstreng
```
Output vil være:
``` PowerShell
Verden
```

### Eksempel 2:
Du kan også angi både startposisjonen og lengden på delstrengen du vil trekke ut.
``` PowerShell
$streng = "Hallo Verden"
$delstreng = $streng.Substring(6, 5)
Write-Output $delstreng
```
Output vil være:
``` PowerShell
Verde
```
---

## Dyp Dykk

Substring metodikk har vært en del av programmeringsspråk lenge, og det er ikke noe unikt for PowerShell. Det gir et kraftig verktøy for å styre og manipulere tekstbaserte data. 

Når det gjelder alternativene, kan `regex`(regular expression), `split` metoden eller fremgangsmåten `indexof` også være nyttige, avhengig av scenarioet. Selv om `.Substring()` er mest brukt, kan det være tilfeller der andre metoder vil være mer passende.

Implementasjonsdetaljene for `.Substring()` er ganske enkle: første argumentet er startposisjonen; det andre argumentet (hvis gitt) er lengden på delstrengen som skal trekkes ut. Hvis lengden ikke er angitt, tar `.Substring()` resten av strengen fra startposisjonen.

---

## Se også:

- MSDN dokumentasjon på `.Substring()`: https://docs.microsoft.com/en-us/dotnet/api/system.string.substring?view=net-5.0

- Forstå strenger i PowerShell mer dybdegående: https://devblogs.microsoft.com/scripting/understanding-powershell-strings/

- Hvis du trenger mer avansert strengmanipulasjon, se `regex`: https://docs.microsoft.com/en-us/dotnet/standard/base-types/regular-expressions

---