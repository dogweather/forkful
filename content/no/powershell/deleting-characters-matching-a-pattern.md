---
title:                "Slette tegn som matcher et mønster"
html_title:           "PowerShell: Slette tegn som matcher et mønster"
simple_title:         "Slette tegn som matcher et mønster"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/powershell/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?

Et av de vanligste oppgavene for en programmerer er å manipulere tekststrenger, enten det er for å analysere data eller formatere utdata. En av måtene å gjøre dette på er å slette eller fjerne bestemte tegn som matcher et mønster. Dette kan være nyttig for å rengjøre data eller for å oppnå ønsket utdataformat. 

## Hvordan:

```PowerShell
# Først må vi velge tekststrengen vi vil manipulere
$tekst = "Hei, dette er en tekststreng!"

# Slette alle komma-tegn fra teksten
$tekst -replace ",", ""
# Output: Hei dette er en tekststreng!

# Slette alle tall fra teksten
$tekst -replace "\d", ""
# Output: Hei, dette er en tekststreng!
```

Generelt sett er syntaxen for å slette tegn som matcher et mønster i PowerShell: `[tekststreng] -replace "[pattern]", "[erstatt]". Dette betyr at vi kan spesifisere hvilket mønster vi vil slette og hva som skal erstatte det.

## Deep Dive

Historisk sett har delete-metoden blitt mye brukt for å fjerne uønsket data når det gjelder å behandle store datamengder. På grunn av at PowerShell er utviklet på bakgrunn av .NET, kan vi også bruke C#-syntax for å slette tegn. Dette gir oss flere alternativer og fleksibilitet når vi arbeider med tekststrenger i PowerShell.

En annen metode for å slette tegn på en mer spesifikk måte er ved hjelp av `regex` (regular expressions). Dette er en kraftig måte å søke etter og manipulere tekststrenger basert på et mønster. Du kan lese mer om `regex` i PowerShell på [Microsoft sin offisielle dokumentasjon](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.core/about/about_regular_expressions?view=powershell-7).

Implementeringsmessig fungerer sletting av tegn ved å finne og bytte dem ut med et tomt tegn eller ønsket tegn. Dette gjøres ved hjelp av .NET's `Replace`-metode som blir kalt når vi bruker `-replace` i PowerShell.

## Se også

[Microsoft sin offisielle dokumentasjon om -replace](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.utility/replace?view=powershell-7)