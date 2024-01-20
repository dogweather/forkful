---
title:                "Rozpoczynanie nowego projektu"
html_title:           "Bash: Rozpoczynanie nowego projektu"
simple_title:         "Rozpoczynanie nowego projektu"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/powershell/starting-a-new-project.md"
---

{{< edit_this_page >}}

# Zaczynając Nowy Projekt w PowerShell

## Co i Dlaczego?

Startowanie nowego projektu to tworzenie nowego, od podstaw systemu kodu, który służy rozwiązaniu określonego problemu. Programiści robią to w celu stworzenia unikalnych rozwiązań dla świeżych problemów lub zastosować nowe podejście do starych problemów.

## Jak to Zrobić:

Zakładając, że masz zainstalowany PowerShell, poniższy kod pokazuje, jak stworzyć nowy skrypt (`MyScript.ps1`). Niech będzie on naszym "projektem". 

```PowerShell
# Utwórz nowy plik
New-Item -ItemType File -Path . -Name MyScript.ps1
```

Kiedy uruchomisz ten kod, powinieneś zobaczyć coś takiego:

```PowerShell
    Directory: C:\Users\YourUsername

Mode                LastWriteTime         Length Name
----                -------------         ------ ----
-a---         10/22/2021   5:15 PM              0 MyScript.ps1
```

## Głębsze Zanurzenie:

Historia: PowerShell powstał w 2006 roku jako projekt Microsoftu dla zarządzania systemami Windows. Od tego czasu zyskał na popularności i jest teraz stosowany do szerokiego zakresu zadań, zarówno na platformach Windows, jak i Linux.

Alternatywy: Alternatywami dla tworzenia nowego projektu w PowerShell są używanie innych języków skryptowych, takich jak Python lub Bash.

Szczegóły implementacji: Nowy projekt w PowerShell zazwyczaj zaczyna się od tworzenia nowego skryptu, ale może również obejmować tworzenie modułów PowerShell, tworzenie funkcji, czy tworzenie procesów automatyzacji.

## Zobacz Też:

- [Dokumentacja PowerShell](https://docs.microsoft.com/pl-pl/powershell/)
- [PowerShell for beginners: Scripts and loops](https://www.computerworld.com/article/2694433/powershell-for-beginners-scripts-and-loops.html)
- [Poradnik Bash-a dla początkujących](https://ryanstutorials.net/bash-scripting-tutorial/)