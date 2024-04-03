---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:20:52.009439-07:00
description: "Att arbeta med CSV-filer (Comma-Separated Values) \xE4r en vanlig uppgift\
  \ f\xF6r att hantera och manipulera data i en strukturerad, tabellform. Programmerare\u2026"
lastmod: '2024-03-13T22:44:38.148409-06:00'
model: gpt-4-0125-preview
summary: "Att arbeta med CSV-filer (Comma-Separated Values) \xE4r en vanlig uppgift\
  \ f\xF6r att hantera och manipulera data i en strukturerad, tabellform."
title: Arbeta med CSV
weight: 37
---

## Vad & Varför?

Att arbeta med CSV-filer (Comma-Separated Values) är en vanlig uppgift för att hantera och manipulera data i en strukturerad, tabellform. Programmerare utför ofta denna operation för att importera, exportera eller manipulera data effektivt för olika applikationer, såsom dataanalys, rapportering eller till och med för att driva webbapplikationer.

## Hur man gör:

### Läsa en CSV-fil

För att läsa från en CSV-fil, använd cmdleten `Import-Csv`. Denna cmdlet läser filen och omvandlar den till anpassade PowerShell-objekt för varje rad.

```powershell
# Importera en CSV-fil
$data = Import-Csv -Path "C:\Data\users.csv"
# Visa innehållet
$data
```

**Exempelutdata:**

```
Name    Age    City
----    ---    ----
John    23     New York
Doe     29     Los Angeles
```

### Skriva till en CSV-fil

För att skriva data till en CSV-fil används cmdleten `Export-Csv`. Denna cmdlet tar emot objekt och omvandlar dem till ett CSV-format.

```powershell
# Skapa ett objekt för export
$users = @(
    [PSCustomObject]@{Name='John'; Age='23'; City='New York'},
    [PSCustomObject]@{Name='Doe'; Age='29'; City='Los Angeles'}
)

# Exportera till en CSV-fil
$users | Export-Csv -Path "C:\Data\new_users.csv" -NoTypeInformation
```

Efter exekvering skapas en fil med namnet `new_users.csv` med den angivna datan.

### Filtrera och manipulera CSV-innehåll

För att filtrera eller manipulera data från en CSV-fil, använd PowerShells objektmanipuleringsmöjligheter. Till exempel, för att välja endast användare över en viss ålder och från en specifik stad:

```powershell
# Importera och filtrera data
$filteredData = Import-Csv -Path "C:\Data\users.csv" | Where-Object {
    $_.Age -gt 25 -and $_.City -eq 'Los Angeles'
}

# Visa filtrerade data
$filteredData
```

**Exempelutdata:**

```
Name    Age    City
----    ---    ----
Doe     29     Los Angeles
```

### Använda Tredjepartsbibliotek

Även om PowerShells inbyggda cmdlets vanligtvis är tillräckliga för vanliga uppgifter, kan mer komplexa operationer dra nytta av tredjepartsbibliotek eller verktyg. Dock, för standardhantering av CSV, såsom att läsa, skriva, filtrera eller sortera, erbjuder PowerShells inbyggda cmdlets som `Import-Csv` och `Export-Csv` vanligtvis robust funktionalitet utan behov av ytterligare bibliotek.
