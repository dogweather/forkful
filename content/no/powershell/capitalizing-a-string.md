---
title:                "Gjøre en streng stor"
html_title:           "PowerShell: Gjøre en streng stor"
simple_title:         "Gjøre en streng stor"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/powershell/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Å gjøre en streng stor er en programmeringspraksis der det første bokstavet i hvert ord i en tekststreng gjøres stor. Programmerere gjøre dette for å forbedre lesbareheten og formatere data riktig.

## Hvordan:
Her er noen kodelinjer og eksempler på hvordan du kan formatere en streng til stor i PowerShell.

```PowerShell
# Definer en streng
$string = "jeg er en streng i powershell"

# Konverter strengen til stor
$capitalString = (Get-Culture).TextInfo.ToTitleCase($string.ToLower())

# Skriv ut den formaterte strengen
Write-Output $capitalString
```

Utførelsen av ovenstående kodeblokk vil produsere følgende utdata:

```PowerShell
Jeg Er En Streng I Powershell
```

## Deep Dive
Historisk sett har programmerere gjort bruk av teknikker for å endre saken for en streng for å forbedre beregningenes nøyaktighet og dataanalyse. I PowerShell kan du også bruke .ToUpper() metoden for å gjøre alle bokstavene i en streng stor. Men for å gjøre den første bokstaven i hver ord stor, er (Get-Culture).TextInfo.ToTitleCase() metoden mer effektiv. Denne metoden holder også hensyn til kulturelle innstillinger, og gir dermed bedre resultat i flerspråklige programmer.

## Se Også
For mer detaljer om tekstmanipulasjon i PowerShell, se de offisielle Microsoft-dokumentene: