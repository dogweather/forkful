---
title:                "Fjerne anførselstegn fra en streng"
date:                  2024-01-26T03:41:37.397621-07:00
model:                 gpt-4-0125-preview
simple_title:         "Fjerne anførselstegn fra en streng"

category:             "PowerShell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/powershell/removing-quotes-from-a-string.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Å fjerne anførselstegn fra en streng i PowerShell fjerner enkle (`'`) eller doble (`"`) anførselstegn som er lagt rundt teksten din. Programmerere trenger ofte å rense strenger for behandling, sammenligning eller utskriftsformål, spesielt når de håndterer brukerinndata eller filparsing.

## Hvordan:
Du kan bruke `-replace` operatøren for å fjerne anførselstegn fra en streng. Slik gjør du det:

```PowerShell
# Erstatt enkle anførselstegn
$stringWithSingleQuotes = "'Hallo, verden!'"
$cleanString = $stringWithSingleQuotes -replace "'", ""
Write-Output $cleanString  # Utdata: Hallo, verden!

# Erstatt doble anførselstegn
$stringWithDoubleQuotes = '"Hallo, verden!"'
$cleanString = $stringWithDoubleQuotes -replace '"', ""
Write-Output $cleanString  # Utdata: Hallo, verden!
```

For begge typer:

```PowerShell
$stringWithQuotes = '"Hei på deg," sa hun.'
$cleanString = $stringWithQuotes -replace "[\"']", ""  # Merk bruk av regex-tegnklasse
Write-Output $cleanString  # Utdata: Hei på deg, sa hun.
```

Eksempelutdata fra konsollen vil se omtrent slik ut:

```
Hallo, verden!
Hallo, verden!
Hei på deg, sa hun.
```

## Dybdeplukking
Tilbake i tiden, før PowerShell var en glimt i Microsofts øye, var tekstbehandling i Windows ofte domenet til batch-skript som hadde begrensede evner. Innføringen av PowerShell brakte med seg kraftfulle strengmanipuleringsfunksjoner som gjorde skripting mye mer robust.

Det finnes alternativer til `-replace`, som å bruke `.Trim()`-metoden for å fjerne anførselstegn kun i starten og slutten av en streng, men de tilbyr ikke den samme kontrollen eller regex-støtten.

```PowerShell
# Bruker .Trim() for anførselstegn på starten og slutten
$stringWithQuotes = '"Hallo, verden!"'
$cleanString = $stringWithQuotes.Trim('"')
Write-Output $cleanString  # Utdata: Hallo, verden!
```

Merk at `-replace` bruker regex i bakgrunnen, så når du arbeider med det, husk at spesielle tegn må unnslippe hvis du målretter dem. Hvis du trenger mer detaljert kontroll over fjerningen av anførselstegn, er å dykke inn i regex med `-replace` veien å gå, noe som gir deg enorm fleksibilitet.

## Se Også
- For mer om regex i PowerShell, sjekk de offisielle dokumentene: [about_Regular_Expressions](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.core/about/about_regular_expressions?view=powershell-7.1)
- Oppdag andre strengmetoder: [Trim(), TrimStart(), TrimEnd()](https://docs.microsoft.com/en-us/dotnet/api/system.string.trim?view=net-6.0)
