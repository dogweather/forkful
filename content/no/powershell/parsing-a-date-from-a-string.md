---
title:                "Tolke en dato fra en streng"
html_title:           "Bash: Tolke en dato fra en streng"
simple_title:         "Tolke en dato fra en streng"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/powershell/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

# Parsing av dato fra streng i PowerShell

## Hva & Hvorfor?
Å parse en dato fra en streng betyr å tolke teksten til et datoobjekt. Dette gjør det mulig for en programmerer å manipulere dater, for eksempel sortere dem eller reformatere dem for bruk i et annet format.

## Hvordan
Det er flere metoder for å parse en dato fra en streng i PowerShell, men en vanlig metode er å bruke "ParseExact"-funksjonen i "DateTime"-klassen. Her er et eksempel:

```PowerShell
$strDato = "2023-02-23"
$datoObjekt = [DateTime]::ParseExact($strDato, "yyyy-MM-dd", $null)
Write-Output $datoObjekt
```

Dette vil gi ut følgende resultat:

```PowerShell
tirsdag 23. februar 2023 00:00:00
```

## Dypdykk
Historisk sett har parsing av datoer fra strenger utgjort særlige utfordringer på grunn av mangfoldet av datoformater som er i bruk rundt om i verden.

Alternativt til "ParseExact", kan du også bruke metoden "TryParse" i "DateTime"-klassen, som vil returnere en boolsk verdi avhengig av om konverteringen har lykkes eller ikke. Dette er ofte en tryggere metode å bruke, da det minsker sannsynligheten for uventede feil.

```PowerShell
$strDato = "23/02/2023"
$isSuksess = [DateTime]::TryParse($strDato, [ref]$datoObjekt)
Write-Output $isSuksess
Write-Output $datoObjekt
```

Hvis strengen kan bli gjort om til en dato, vil dette gi 'True' og den parsed datoobjektet som output.

## Se også
For mer informasjon, se følgende lenker:

- Microsofts dokumentasjon på `ParseExact`: [https://docs.microsoft.com/dotnet/api/system.datetime.parseexact](https://docs.microsoft.com/dotnet/api/system.datetime.parseexact)
- Microsofts dokumentasjon på `TryParse`: [https://docs.microsoft.com/dotnet/api/system.datetime.tryparse](https://docs.microsoft.com/dotnet/api/system.datetime.tryparse)