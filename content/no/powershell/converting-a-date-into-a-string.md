---
title:                "Konvertere en dato til en streng"
html_title:           "PowerShell: Konvertere en dato til en streng"
simple_title:         "Konvertere en dato til en streng"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/powershell/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

Det å konvertere en dato til en streng er en viktig del av programmering som tillater oss å representere datoer på en meningsfull måte. Dette er spesielt nyttig når vi ønsker å vise datoer på en bestemt måte, for eksempel i et bestemt format eller språk. 

## Hva & hvorfor?
Konvertering av en dato til en streng betyr rett og slett å endre hvordan datoen blir presentert. I stedet for å vise datoen som tall eller i et standardformat, kan vi konvertere den til et mer leselig og forståelig format. Dette gjør det lettere for både deg som programmerer og for brukeren av programmet å forstå datoen som vises. 

## Slik gjør du det:
```PowerShell
Get-Date -Format fi-FI
```
Eksempel på utdata:
```
torstai 25. joulukuuta 2020 19.44.25
```
I dette eksempelet blir datoen konvertert til et finsk format, med datoen og tidspunktet skrevet ut både på finsk og med måneden som navn i stedet for som tall. Dette er bare ett av mange formater som kan brukes, basert på de språk- og datoinnstillingene som er tilgjengelige.

## Dypdykk:
Date formatting is not a new concept. It dates back to the early days of computing, when date and time data had to be stored and displayed using limited memory and character sets. As programming languages and platforms evolved, so did the methods for converting dates into strings. In PowerShell, the Get-Date cmdlet allows for a wide range of formatting options, including the ability to customize the date and time formats. There are also alternative approaches to formatting dates in PowerShell, such as using .NET formatting methods or external libraries. 

## Se også:
[PowerShell Get-Date cmdlet documentation](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.utility/get-date?view=powershell-7.1)

[Built-in .NET formatting methods in PowerShell](https://docs.microsoft.com/en-us/dotnet/standard/base-types/custom-date-and-time-format-strings)