---
title:                "Praca z plikami CSV"
date:                  2024-01-19
html_title:           "Bash: Praca z plikami CSV"
simple_title:         "Praca z plikami CSV"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/powershell/working-with-csv.md"
---

{{< edit_this_page >}}

## Co i dlaczego?
CSV to prosty format pliku do przechowywania tabelarnych danych. W PowerShellu często używamy CSV, bo to uniwersalny format – łatwy do odczytu i pisania zarówno dla ludzi, jak i maszyn.

## Jak to zrobić:

### Importowanie danych z pliku CSV
```PowerShell
$csvData = Import-Csv -Path 'ścieżka/do/pliku.csv'
$csvData
```

### Eksportowanie danych do pliku CSV
```PowerShell
$csvData | Export-Csv -Path 'ścieżka/do/nowego_pliku.csv' -NoTypeInformation
```

### Dodawanie wierszy do istniejącego pliku CSV
```PowerShell
$newRow = [PSCustomObject]@{Name='Jan'; Age=30}
$newRow | Export-Csv -Path 'ścieżka/do/pliku.csv' -Append -NoTypeInformation
```

### Filtrowanie i selekcja danych
```PowerShell
$csvData | Where-Object { $_.Age -gt 25 } | Select-Object -Property Name, Age
```

## Deep Dive

CSV (Comma Separated Values) to format pojawiający się w latach 70. Prostota CSV sprawia, że jest kompatybilny z wieloma systemami i aplikacjami. Alternatywy dla CSV obejmują JSON, XML czy bazy danych SQLite. W PowerShellu, cmdlety takie jak `Import-Csv` i `Export-Csv` wykorzystują klasy .NET do przetwarzania danych, co zapewnia wydajność i elastyczność.

## Zobacz też

- Oficjalna dokumentacja PowerShell dla `Import-Csv`: https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.utility/import-csv
- Oficjalna dokumentacja PowerShell dla `Export-Csv`: https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.utility/export-csv
- Artykuł na temat zaawansowanych technik pracy z CSV w PowerShellu: https://devblogs.microsoft.com/powershell/working-with-csv-files-in-powershell/
