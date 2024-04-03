---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:08:13.137785-07:00
description: "Jak to zrobi\u0107: PowerShell oferuje prosty spos\xF3b na sprawdzenie\
  \ obecno\u015Bci katalogu za pomoc\u0105 polecenia `Test-Path`. To polecenie zwraca\
  \ warto\u015B\u0107 logiczn\u0105\u2026"
lastmod: '2024-03-13T22:44:35.644380-06:00'
model: gpt-4-0125-preview
summary: "PowerShell oferuje prosty spos\xF3b na sprawdzenie obecno\u015Bci katalogu\
  \ za pomoc\u0105 polecenia `Test-Path`."
title: Sprawdzanie, czy katalog istnieje
weight: 20
---

## Jak to zrobić:
PowerShell oferuje prosty sposób na sprawdzenie obecności katalogu za pomocą polecenia `Test-Path`. To polecenie zwraca wartość logiczną wskazującą, czy określona ścieżka istnieje. Oto jak możesz tego użyć:

```powershell
# Sprawdź, czy katalog istnieje
$directoryPath = "C:\ExamplePath"
$directoryExists = Test-Path -Path $directoryPath
Write-Output "Czy katalog istnieje? $directoryExists"
```

Przykładowe wyjście dla istniejącego katalogu:

```
Czy katalog istnieje? True
```

I dla katalogu, który nie istnieje:

```
Czy katalog istnieje? False
```

Dla bardziej złożonych skryptów, zwłaszcza tych interaktywnych z udziałami sieciowymi lub przechowywaniem w chmurze, mogą być potrzebne dodatkowe sprawdzenia lub funkcjonalności, które nie są bezpośrednio dostępne przez `Test-Path`. W takich przypadkach korzystanie z zewnętrznych modułów PowerShell lub bibliotek może być korzystne, chociaż większość rutynowych zadań można wykonać przy użyciu wbudowanych poleceń PowerShell. Do ostatniej aktualizacji mojej wiedzy nie pojawiła się szeroko przyjęta biblioteka stron trzecich specjalnie do sprawdzania istnienia katalogu poza tym, co oferuje `Test-Path`, głównie dlatego, że `Test-Path` sam w sobie jest zarówno solidny, jak i efektywny w tym celu.
