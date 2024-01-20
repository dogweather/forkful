---
title:                "Drukowanie komunikatów debugowania"
html_title:           "Haskell: Drukowanie komunikatów debugowania"
simple_title:         "Drukowanie komunikatów debugowania"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/powershell/printing-debug-output.md"
---

{{< edit_this_page >}}

## Co i Dlaczego?

Drukowanie informacji debugowania, to sposób na logowanie i śledzenie aktywności w programie - to jak zrobienie rentgena kodu. Programiści robią to, aby łatwiej było im znaleźć i naprawić błędy.

## Jak to zrobić:

```PowerShell
# Używając cmdlet Write-Debug
$DebugPreference = 'Continue'
Write-Debug "To jest moja wiadomość debugowania"
```

Po uruchomieniu tego kodu, otrzymasz następujący wynik:

```PowerShell
DEBUG: To jest moja wiadomość debugowania
```

Inny sposób to używanie 'Write-Host' z parametrem '-Debug':

```PowerShell
Write-Host "To jest inny sposób na debugowanie" -Debug
```

Efekt będzie taki sam - wyświetli się wiadomość debugowania.

## Deep Dive:

Historia debugowania sięga lat '40. i '50., kiedy programiści szukali fizycznych błędów w swoich maszynach.

Współcześnie mamy różne techniki debugowania. Oprocz `Write-Debug`, jest jeszcze np. `Set-PSBreakpoint`, który umożliwia ustawianie punktów przerwania. Dodatkowo, `Write-Debug` jest wbudowanym cmdletem w PowerShell, więc nie potrzebujesz żadnych dodatkowych narzędzi.

Ważne jest jednak, aby nie nadużywać drukowania debugowania - za dużo danych może przeszkadzać w szybkim znalezieniu problemu. Dobrą praktyką jest wyłączanie logowania debugowania, gdy kod jest gotowy do produkcji.

## Zobacz także:

- Kompletny przewodnik po Debugging w PowerShell: https://mcpmag.com/articles/2018/09/06/debugging-with-powershell.aspx
- Oficjalna dokumentacja PowerShell na temat Write-Debug: https://docs.microsoft.com/pl-pl/powershell/module/microsoft.powershell.utility/write-debug?view=powershell-7.1
- Zrozumienie debugowania w PowerShell: https://adamtheautomator.com/powershell-debugging/