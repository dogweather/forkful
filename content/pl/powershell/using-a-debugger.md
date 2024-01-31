---
title:                "Korzystanie z debugera"
date:                  2024-01-26T04:08:55.992040-07:00
model:                 gpt-4-0125-preview
simple_title:         "Korzystanie z debugera"

category:             "PowerShell"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/powershell/using-a-debugger.md"
---

{{< edit_this_page >}}

## Co i dlaczego?
Używanie debugera oznacza ustawianie punktów przerwania, przeglądanie kodu krok po kroku, obserwowanie zmiennych oraz inspekcję stanu programu podczas jego wykonania. Dla programistów jest to zmiana gry, ponieważ pozwala na zlokalizowanie błędów i lepsze zrozumienie działania naszego kodu.

## Jak to zrobić:
W PowerShellu możesz debugować skrypty za pomocą wbudowanego środowiska PowerShell Integrated Scripting Environment (ISE) lub Visual Studio Code (VS Code) z rozszerzeniem PowerShell. Oto jak używać punktów przerwania w obu przypadkach:

### PowerShell ISE:
```PowerShell
# Ustaw punkt przerwania na określonej linii
Set-PSBreakpoint -Script .\MyScript.ps1 -Line 5

# Uruchom swój skrypt normalnie
.\MyScript.ps1

# Kiedy skrypt natrafi na punkt przerwania, możesz inspekcjonować zmienne
$myVariable

# Kontynuuj wykonanie
Continue
```

### Visual Studio Code:
```PowerShell
# Otwórz swój skrypt PowerShell w VS Code.
# Kliknij po lewej stronie numeru linii, aby ustawić punkt przerwania.
# Rozpocznij debugowanie, naciskając F5 lub klikając 'Rozpocznij debugowanie'.

# VS Code zatrzyma wykonanie na twoim punkcie przerwania.
# Użyj panelu debugowania, aby obserwować zmienne, inspekcjonować stos wywołań i kontrolować przepływ.
```

Debugowanie w obu środowiskach pozwala na wejście w kod (F11), krok nad kodem (F10) oraz wyjście z kodu (Shift+F11) podczas debugowania.

## Zagłębienie się
Historia debugowania w PowerShellu była nieco niewygodna; wymagała wielu linii `Write-Host` do wyjścia stanów zmiennych lub klasycznej metody prób i błędów. Z pojawieniem się PowerShell ISE, a niedawno również VS Code z jego bogatymi funkcjami debugowania, debugowanie w PowerShellu stało się niemal równie intuicyjne, jak w pełnoprawnych językach programowania.

Alternatywami dla natywnych narzędzi debugowania PowerShell są narzędzia firm trzecich, takie jak PowerGUI lub używanie rozbudowanych IDE takich jak Visual Studio z wtyczką PowerShell.

Przy implementacji debugera warto zwrócić uwagę na zakres skryptu, szczególnie przy pracy ze skryptami źródłowymi lub modułami. Punkty przerwania mogą być oparte na warunkach, zmianach zmiennych lub konkretnych liniach, co pozwala na precyzyjną kontrolę podczas sesji debugowania.

Ponadto, z przejściem na PowerShell Core (PowerShell wieloplatformowy), debugowanie w dużej mierze przeszło w ręce VS Code, które zapewnia spójne doświadczenie na różnych platformach.

## Zobacz również
Aby dowiedzieć się więcej o debugowaniu w PowerShell:
- [about_Debuggers](https://docs.microsoft.com/pl-pl/powershell/module/microsoft.powershell.core/about/about_Debuggers)
