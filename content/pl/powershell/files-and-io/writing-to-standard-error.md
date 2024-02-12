---
title:                "Pisanie do standardowego błędu"
aliases: - /pl/powershell/writing-to-standard-error.md
date:                  2024-01-19
simple_title:         "Pisanie do standardowego błędu"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/powershell/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Co i dlaczego?

W PowerShellu zapis do strumienia błędów (standard error) pozwala odseparować normalne wyniki działania skryptu od komunikatów o błędach. Programiści używają tego, aby łatwiej zarządzać i identyfikować problemy podczas wykonywania kodu.

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
