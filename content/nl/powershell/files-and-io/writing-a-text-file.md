---
title:                "Een tekstbestand schrijven"
aliases:
- /nl/powershell/writing-a-text-file/
date:                  2024-01-28T22:12:55.124870-07:00
model:                 gpt-4-0125-preview
simple_title:         "Een tekstbestand schrijven"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/nl/powershell/writing-a-text-file.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Wat & Waarom?
Een tekstbestand schrijven is gegevens opslaan als platte tekst op schijf. Programmeurs doen dit voor logboekregistratie, configuratie of het opslaan van gebruikersgegevens. Het is basis maar cruciaal voor de meeste applicaties.

## Hoe te:
Hier is hoe je naar een tekstbestand schrijft in PowerShell; het is supergemakkelijk!

Een nieuw bestand maken en tekst daarnaar schrijven:
```PowerShell
"Hello, world!" | Out-File -FilePath .\hello.txt
```

Tekst toevoegen aan een bestaand bestand:
```PowerShell
"Welcome to PowerShell scripting!" | Add-Content -Path .\hello.txt
```

Controleer de inhoud van het bestand:
```PowerShell
Get-Content .\hello.txt
```

Voorbeelduitvoer:
```
Hello, world!
Welcome to PowerShell scripting!
```

## Diepgaande duik
PowerShell-bestanden gebruiken standaard UTF-16 codering. Historisch gezien waren tekstbestanden eenvoudiger—alleen ASCII. Nu laten `Out-File` en `Add-Content` je de codering kiezen. Als je van de oude stempel bent, bestaat `Set-Content`, maar dat heeft beperkingen. Overweeg voor grotere bestanden `[System.IO.StreamWriter]` voor efficiëntie.

## Zie ook
Voor meer PowerShell-bestandsbeheer kunde, bezoek:
- Microsoft Docs over [Out-File](https://docs.microsoft.com/nl-nl/powershell/module/microsoft.powershell.utility/out-file)
- Microsoft Docs over [Add-Content](https://docs.microsoft.com/nl-nl/powershell/module/microsoft.powershell.management/add-content)
  
Onthoud, oefening baart kunst. Dus begin met scripten!
