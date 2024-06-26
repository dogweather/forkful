---
date: 2024-01-20 17:41:12.849434-07:00
description: "Hvordan: For \xE5 lage en midlertidig fil i PowerShell, bruk `New-TemporaryFile`\
  \ cmdlet. Dette gir en enkel fil i systemets temp-mappe."
lastmod: '2024-03-13T22:44:41.036346-06:00'
model: gpt-4-1106-preview
summary: "For \xE5 lage en midlertidig fil i PowerShell, bruk `New-TemporaryFile`\
  \ cmdlet."
title: Opprette en midlertidig fil
weight: 21
---

## Hvordan:
For å lage en midlertidig fil i PowerShell, bruk `New-TemporaryFile` cmdlet. Dette gir en enkel fil i systemets temp-mappe.

```PowerShell
$tempFile = New-TemporaryFile
Write-Output "Temp-fil opprettet: $($tempFile.FullName)"
```

Sample output:

```
Temp-fil opprettet: C:\Users\<DinBruker>\AppData\Local\Temp\tmpXXXX.tmp
```

For å skrive til den midlertidige filen:

```PowerShell
Set-Content -Path $tempFile.FullName -Value 'Dette er testinnhold.'
```

For å lese fra den midlertidige filen:

```PowerShell
Get-Content -Path $tempFile.FullName
```

For å slette den midlertidige filen når du er ferdig:

```PowerShell
Remove-Item -Path $tempFile.FullName
```

## Deep Dive
Opprettelse av midlertidige filer går tilbake til de tidligste dagene av programmering. De tillater at data kan behandles uten å forstyrre hovedlagringsområdene. Før `New-TemporaryFile` cmdlet, kunne programmerere bruke `[System.IO.Path]::GetTempFileName()` metoden i .NET for å oppnå lignende resultater.

Alternativer for å lage en midlertidig fil inkluderer bruk av `Out-File` med en manuelt generert filbane eller bruk av en tredjepartmodul. 

Når det gjelder implementasjon, er det viktig å merke seg at midlertidige filer skal slettes etter bruk for å forhindre oppsamling av unødvendige filer, som kan føre til problemer med diskplass og personvern.

## Se også
- [PowerShell dokumentasjon for New-TemporaryFile](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.utility/new-temporaryfile)
- [.NET dokumentasjon for Path.GetTempFileName](https://docs.microsoft.com/en-us/dotnet/api/system.io.path.gettempfilename)
