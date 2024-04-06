---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:20:53.731117-07:00
description: "Hvordan: For \xE5 lese fra en CSV-fil, bruk `Import-Csv` cmdleten. Denne\
  \ cmdleten leser filen og konverterer den til egendefinerte PowerShell-objekter\
  \ for\u2026"
lastmod: '2024-03-13T22:44:41.039341-06:00'
model: gpt-4-0125-preview
summary: "For \xE5 lese fra en CSV-fil, bruk `Import-Csv` cmdleten."
title: Arbeide med CSV
weight: 37
---

## Hvordan:


### Lese en CSV-fil
For å lese fra en CSV-fil, bruk `Import-Csv` cmdleten. Denne cmdleten leser filen og konverterer den til egendefinerte PowerShell-objekter for hver rad.

```powershell
# Importere en CSV-fil
$data = Import-Csv -Path "C:\Data\users.csv"
# Vise innholdet
$data
```

**Eksempel på utdata:**

```
Navn    Alder    By
----    -----    ---
John    23      New York
Doe     29      Los Angeles
```

### Skrive til en CSV-fil
På den andre siden, for å skrive data til en CSV-fil, brukes `Export-Csv` cmdleten. Denne cmdleten tar inndataobjekter og konverterer dem til et CSV-format.

```powershell
# Lage et objekt for eksport
$users = @(
    [PSCustomObject]@{Navn='John'; Alder='23'; By='New York'},
    [PSCustomObject]@{Navn='Doe'; Alder='29'; By='Los Angeles'}
)

# Eksportere til en CSV-fil
$users | Export-Csv -Path "C:\Data\new_users.csv" -NoTypeInformation
```

Etter utførelse, en fil med navnet `new_users.csv` er opprettet med den angitte dataen.

### Filtrering og manipulering av CSV-innhold
For å filtrere eller manipulere data fra en CSV-fil, bruk PowerShell sine objektmanipuleringsegenskaper. For eksempel, for å kun velge brukere over en viss alder og fra en spesifikk by:

```powershell
# Importere og filtrere data
$filteredData = Import-Csv -Path "C:\Data\users.csv" | Where-Object {
    $_.Alder -gt 25 -og $_.By -eq 'Los Angeles'
}

# Vise filtrerte data
$filteredData
```

**Eksempel på utdata:**

```
Navn    Alder    By
----    -----    ---
Doe     29      Los Angeles
```

### Bruk av tredjepartsbiblioteker
Selv om PowerShell sine innebygde cmdleter vanligvis er tilstrekkelige for vanlige oppgaver, kan mer komplekse operasjoner ha nytte av tredjepartsbiblioteker eller verktøy. Imidlertid, for standard CSV-manipulasjon, som lesing, skriving, filtrering eller sortering, tilbyr vanligvis PowerShell sine innebygde cmdleter robust funksjonalitet uten behov for ekstra biblioteker.
