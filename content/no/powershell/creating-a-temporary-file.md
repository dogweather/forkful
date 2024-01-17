---
title:                "Oppretting av midlertidig fil"
html_title:           "PowerShell: Oppretting av midlertidig fil"
simple_title:         "Oppretting av midlertidig fil"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/powershell/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Å opprette en midlertidig fil betyr å lage en midlertidig lagringsplass for data som trengs for en kort periode. Programmører gjør dette for å effektivt håndtere data og samarbeide med andre programmer.

## Hvordan:
```powershell
# Eksempel på å opprette en midlertidig fil og skrive til den
$TempFile = New-TemporaryFile
"Skriver til midlertidig fil: $Tempfile" | Out-File $TempFile.FullName

# Eksempel på å lese data fra midlertidig fil
$Content = Get-Content $TempFile.FullName
Write-Host "Data fra midlertidig fil:" $Content

# Eksempel på å slette midlertidig fil
Remove-Item $TempFile.FullName
```

Output:
Skriver til midlertidig fil: C:\Users\<Ditt brukernavn>\AppData\Local\Temp\tmpABCD.tmp
Data fra midlertidig fil: Dette er data fra midlertidig fil.

## Dypdykk:
Å opprette midlertidige filer er en måte å håndtere data på som har vært brukt i mange år innen systemprogrammering. Alternativene til å bruke midlertidige filer kan være å bruke minnet for å lagre data, men det kan føre til lekkasjer og andre problemer. Implementeringen av midlertidige filer er enkelt og effektivt i PowerShell, noe som gjør det til et vanlig verktøy for programmerere.

## Se også:
- [Microsoft Docs: New-TemporaryFile](https://docs.microsoft.com/powershell/module/microsoft.powershell.utility/new-temporaryfile?view=powershell-7)
- [TechNet: Working with temporary files in PowerShell](https://social.technet.microsoft.com/wiki/contents/articles/648.working-with-temporary-files-in-powershell.aspx)