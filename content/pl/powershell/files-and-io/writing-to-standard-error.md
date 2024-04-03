---
date: 2024-01-19
description: "Jak to zrobi\u0107? ."
lastmod: '2024-03-13T22:44:35.646320-06:00'
model: unknown
summary: .
title: "Pisanie do standardowego b\u0142\u0119du"
weight: 25
---

## Jak to zrobić?
```PowerShell
# Zapis do strumienia błędów za pomocą Write-Error
Write-Error "To jest komunikat błędu"

# Zapis do strumienia błędów przez przekierowanie
"Coś poszło nie tak" 1>&2

# Własna funkcja pisząca do strumienia błędów
function Write-StdErr($message) {
    $message | Out-File -FilePath 'php://stderr'
}
Write-StdErr "To jest błąd z własnej funkcji"
```
W przykładach powyżej każdy fragment kodu zapisuje informacje do strumienia błędów. `Write-Error` jest wbudowanym poleceniem. Przekierowanie z `1>&2` to klasyczne przekierowanie standardowego wyjścia (1) do standardowego błędu (2). Natomiast funkcja `Write-StdErr` pokazuje inny sposób zapisu do tego strumienia.

## Deep Dive
Historia: PowerShell, tak jak wiele shelle'ów, dziedziczy ideę przekierowania i strumieni z modelu Unixowego. Standardowe strumienie (stdout, stderr, stdin) pochodzą z lat 70-tych.

Alternatywy: Można także używać `Throw` w celu wygenerowania wyjątku, co również trafia do standard error, ale zatrzymuje wykonanie skryptu.

Szczegóły implementacji: Strumień błędów w PowerShellu to jeden z pięciu podstawowych strumieni: Output, Error, Warning, Verbose i Debug. Każdy strumień można przekierować lub ukryć niezależnie.

## Zobacz również
- [about_Redirection](https://docs.microsoft.com/powershell/module/microsoft.powershell.core/about/about_redirection)
- [Write-Error](https://docs.microsoft.com/powershell/module/microsoft.powershell.utility/write-error)
- [about_Throw](https://docs.microsoft.com/powershell/module/microsoft.powershell.core/about/about_throw)
