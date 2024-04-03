---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:15:09.966714-07:00
description: "Parsowanie daty z ci\u0105gu tekstowego polega na rozpoznawaniu i konwersji\
  \ zapisanych dat w formie tekstowej na typ danych daty, kt\xF3ry PowerShell mo\u017C\
  e\u2026"
lastmod: '2024-03-13T22:44:35.639378-06:00'
model: gpt-4-0125-preview
summary: "Parsowanie daty z ci\u0105gu tekstowego polega na rozpoznawaniu i konwersji\
  \ zapisanych dat w formie tekstowej na typ danych daty, kt\xF3ry PowerShell mo\u017C\
  e zrozumie\u0107 i z kt\xF3rym mo\u017Ce pracowa\u0107."
title: "Analiza sk\u0142adniowa daty z \u0142a\u0144cucha znak\xF3w"
weight: 30
---

## Jak to zrobić:
PowerShell ułatwia parsowanie dat z ciągów tekstowych za pomocą polecenia cmdlet `Get-Date` oraz akceleratora typu `[datetime]`, które dobrze radzą sobie ze standardowymi formatami daty. Dla bardziej skomplikowanych lub niestandardowych ciągów dat, można wykorzystać metodę `[datetime]::ParseExact`, aby określić dokładny format.

### Użycie `Get-Date` i `[datetime]`:
```powershell
# Prosta konwersja przy użyciu Get-Date
$stringDate = "2023-04-01"
$date = Get-Date $stringDate
echo $date
```
**Przykładowy wynik:**
```
Saturday, April 1, 2023 12:00:00 AM
```

```powershell
# Korzystanie z akceleratora typu [datetime]
$stringDate = "April 1, 2023"
$date = [datetime]$stringDate
echo $date
```
**Przykładowy wynik:**
```
Saturday, April 1, 2023 12:00:00 AM
```

### Użycie `[datetime]::ParseExact` dla niestandardowych formatów:
Dla formatów nie rozpoznawanych automatycznie, można zdefiniować dokładny format, aby zapewnić poprawne parsowanie.
```powershell
$stringDate = "01-04-2023 14:00"
$format = "dd-MM-yyyy HH:mm"
$culture = [Globalization.CultureInfo]::InvariantCulture
$date = [datetime]::ParseExact($stringDate, $format, $culture)
echo $date
```
**Przykładowy wynik:**
```
Saturday, April 1, 2023 2:00:00 PM
```

### Wykorzystanie bibliotek stron trzecich
Chociaż PowerShell sam w sobie jest dość potężny do parsowania dat, w bardzo skomplikowanych scenariuszach lub w przypadku dodatkowych funkcjonalności, można rozważyć wykorzystanie bibliotek .NET, takich jak NodaTime, chociaż dla wielu typowych przypadków użycia, wystarczające będą natywne możliwości PowerShell.

```powershell
# Używanie NodaTime tylko jako ilustracja, zauważ, że musisz dodać bibliotekę do swojego projektu
# Install-Package NodaTime -Version 3.0.5
# Używanie NodaTime do parsowania daty
[string]$stringDate = "2023-04-01T14:00:00Z"
[NodaTime.Instant]::FromDateTimeUtc([datetime]::UtcNow)
[NodaTime.LocalDate]$localDate = [NodaTime.LocalDate]::FromDateTime([datetime]::UtcNow)
echo $localDate
```
**Uwaga przykładowa:** Powyższy kod jest ilustracją koncepcyjną. W praktyce upewnij się, że NodaTime jest poprawnie dodane do twojego projektu, aby typy i metody były dostępne.
