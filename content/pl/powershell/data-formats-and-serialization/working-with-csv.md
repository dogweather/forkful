---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:20:56.248166-07:00
description: "Praca z plikami CSV (Comma-Separated Values - warto\u015Bci oddzielone\
  \ przecinkami) to cz\u0119ste zadanie podczas zarz\u0105dzania i manipulowania danymi\
  \ w\u2026"
lastmod: '2024-03-13T22:44:35.652356-06:00'
model: gpt-4-0125-preview
summary: "Praca z plikami CSV (Comma-Separated Values - warto\u015Bci oddzielone przecinkami)\
  \ to cz\u0119ste zadanie podczas zarz\u0105dzania i manipulowania danymi w\u2026"
title: Praca z plikami CSV
---

{{< edit_this_page >}}

## Co i dlaczego?

Praca z plikami CSV (Comma-Separated Values - wartości oddzielone przecinkami) to częste zadanie podczas zarządzania i manipulowania danymi w strukturalnej, tabelarycznej formie. Programiści często wykonują tę operację, aby importować, eksportować lub efektywnie manipulować danymi dla różnych aplikacji, takich jak analiza danych, raportowanie, czy nawet zasilanie aplikacji internetowych.

## Jak to zrobić:

### Odczytywanie pliku CSV

Aby odczytać plik CSV, użyj cmdletu `Import-Csv`. Ten cmdlet odczytuje plik i konwertuje go na niestandardowe obiekty PowerShell dla każdego wiersza.

```powershell
# Importowanie pliku CSV
$data = Import-Csv -Path "C:\Data\users.csv"
# Wyświetlanie zawartości
$data
```

**Przykładowy wynik:**

```
Name    Age    City
----    ---    ----
John    23     New York
Doe     29     Los Angeles
```

### Zapisywanie do pliku CSV

Odwracając sytuację, aby zapisać dane do pliku CSV, używany jest cmdlet `Export-Csv`. Ten cmdlet pobiera obiekty wejściowe i konwertuje je na format CSV.

```powershell
# Tworzenie obiektu do eksportu
$users = @(
    [PSCustomObject]@{Name='John'; Age='23'; City='New York'},
    [PSCustomObject]@{Name='Doe'; Age='29'; City='Los Angeles'}
)

# Eksportowanie do pliku CSV
$users | Export-Csv -Path "C:\Data\new_users.csv" -NoTypeInformation
```

Po wykonaniu, plik o nazwie `new_users.csv` zostaje utworzony z podanymi danymi.

### Filtrowanie i manipulowanie zawartością CSV

Aby filtrować lub manipulować danymi z pliku CSV, używaj możliwości manipulacji obiektami w PowerShell. Na przykład, aby wybrać tylko użytkowników powyżej pewnego wieku i z określonego miasta:

```powershell
# Importowanie i filtrowanie danych
$filteredData = Import-Csv -Path "C:\Data\users.csv" | Where-Object {
    $_.Age -gt 25 -and $_.City -eq 'Los Angeles'
}

# Wyświetlanie filtrowanych danych
$filteredData
```

**Przykładowy wynik:**

```
Name    Age    City
----    ---    ----
Doe     29     Los Angeles
```

### Użycie bibliotek firm trzecich

Chociaż natywne cmdlety PowerShell zazwyczaj wystarczają do zadań typowych, bardziej złożone operacje mogą korzystać z bibliotek lub narzędzi firm trzecich. Jednak dla standardowej manipulacji CSV, takiej jak odczyt, zapis, filtrowanie czy sortowanie, wbudowane cmdlety PowerShell, takie jak `Import-Csv` i `Export-Csv`, zazwyczaj oferują solidną funkcjonalność bez potrzeby korzystania z dodatkowych bibliotek.
