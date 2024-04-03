---
date: 2024-01-26 04:08:55.992040-07:00
description: "Jak to zrobi\u0107: W PowerShellu mo\u017Cesz debugowa\u0107 skrypty\
  \ za pomoc\u0105 wbudowanego \u015Brodowiska PowerShell Integrated Scripting Environment\
  \ (ISE) lub Visual Studio\u2026"
lastmod: '2024-03-13T22:44:35.634450-06:00'
model: gpt-4-0125-preview
summary: "W PowerShellu mo\u017Cesz debugowa\u0107 skrypty za pomoc\u0105 wbudowanego\
  \ \u015Brodowiska PowerShell Integrated Scripting Environment (ISE) lub Visual Studio\
  \ Code (VS Code) z rozszerzeniem PowerShell."
title: Korzystanie z debugera
weight: 35
---

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
