---
title:                "Arbeide med CSV"
aliases: - /no/powershell/working-with-csv.md
date:                  2024-02-03T19:20:53.731117-07:00
model:                 gpt-4-0125-preview
simple_title:         "Arbeide med CSV"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/powershell/working-with-csv.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Hva og hvorfor?

Å jobbe med CSV-filer (Comma-Separated Values) er en vanlig oppgave for å håndtere og manipulere data i en strukturert, tabellform. Programmerere utfører ofte denne operasjonen for å importere, eksportere eller manipulere data effektivt for ulike applikasjoner, slik som dataanalyse, rapportering eller til og med for å drive webapplikasjoner.

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
