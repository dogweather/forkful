---
date: 2024-01-20 17:41:06.466232-07:00
description: "S\xE5 h\xE4r g\xF6r du: Skapa en tempor\xE4r fil i PowerShell."
lastmod: '2024-03-13T22:44:38.144999-06:00'
model: gpt-4-1106-preview
summary: "Skapa en tempor\xE4r fil i PowerShell."
title: "Skapa en tempor\xE4r fil"
weight: 21
---

## Så här gör du:
Skapa en temporär fil i PowerShell:

```PowerShell
$tempFile = [System.IO.Path]::GetTempFileName()
"Det här är testdata" | Out-File -FilePath $tempFile

# Skriv ut innehållet i den temporära filen
Get-Content -Path $tempFile
```

Rensa upp genom att ta bort den temporära filen:

```PowerShell
Remove-Item -Path $tempFile
```

## Fördjupning
Att skapa temporära filer är inte något nytt. I tidiga operativsystem, som UNIX, används `/tmp`-katalogen ofta för detta syfte. Powershell erbjuder `[System.IO.Path]::GetTempFileName()` för att automatiskt skapa en säkert namngiven temporär fil i systemets temp-katalog.

Alternativ till Powershell-inbyggda kommandon inkluderar direkt användning av .NET-funktioner eller tredjepartsskript. För att uppnå bättre kontroll eller säkerhet kan en anpassad funktion skrivas som skapar en temporär fil i en specifik mapp eller med särskilda attribut.

## Se även
- Microsoft Docs om `GetTempFileName`-metoden: [https://docs.microsoft.com/en-us/dotnet/api/system.io.path.gettempfilename](https://docs.microsoft.com/en-us/dotnet/api/system.io.path.gettempfilename)
- PowerShell Documentation: [https://docs.microsoft.com/en-us/powershell/](https://docs.microsoft.com/en-us/powershell/)
