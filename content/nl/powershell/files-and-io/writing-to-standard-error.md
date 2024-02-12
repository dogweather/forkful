---
title:                "Schrijven naar standaardfout"
aliases:
- /nl/powershell/writing-to-standard-error.md
date:                  2024-01-28T22:13:34.661909-07:00
model:                 gpt-4-0125-preview
simple_title:         "Schrijven naar standaardfout"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/nl/powershell/writing-to-standard-error.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Wat & Waarom?
Schrijven naar standaardfout (stderr) stuurt foutmeldingen en diagnostische berichten apart van standaarduitvoer (stdout). Programmeurs doen dit om reguliere programma-uitvoer netjes te scheiden van foutinformatie, wat het debuggen en loggen makkelijker maakt.

## Hoe te:
```PowerShell
# Schrijf een eenvoudige fout naar stderr
Write-Host "Oeps, er is een fout opgetreden!" -ForegroundColor Red 1>&2

# Schrijf een fout met behulp van het Write-Error cmdlet
Write-Error "Dit is een foutmelding!"

# Gebruik $ErrorView om fouten anders weer te geven of te behandelen
$ErrorView = "CategoryView"
probeer {
    Get-ChildItem "nonexistentfile.txt"
} vang {
    Write-Host $_.Exception.Message -ForegroundColor Red 1>&2
}
```

Voorbeelduitvoer:
```
Oeps, er is een fout opgetreden!
Write-Error: Dit is een foutmelding!
Get-ChildItem: Kan het pad 'C:\...\nonexistentfile.txt' niet vinden omdat het niet bestaat.
```

## Diepere Duik
Historisch gezien heeft het scheiden van stdout en stderr Unix-roots, waardoor gebruikers uitvoer apart kunnen omleiden. PowerShell, dat dit concept erft, gebruikt Write-Error en Write-Host (met een omleiding), onder andere cmdlets, om berichten naar stderr te sturen. Onder de motorkap, wikkelt PowerShell .NET-methoden om deze functie te implementeren.

Alternatieven zijn onder andere het gebruik van throw statements of exception handling blocks; deze beïnvloeden echter de scriptstroom. Schrijven naar stderr onderbreekt de uitvoering niet, tenzij je specifiek de $Error variabele controleert of -ErrorAction parameters gebruikt.

## Zie Ook
- [about_Redirection](https://docs.microsoft.com/powershell/module/microsoft.powershell.core/about/about_redirection)
- [Write-Error](https://docs.microsoft.com/powershell/module/microsoft.powershell.utility/write-error)
- [about_Try_Catch_Finally](https://docs.microsoft.com/nl-nl/powershell/scripting/learn/deep-dives/everything-about-exceptions?view=powershell-7.1)
