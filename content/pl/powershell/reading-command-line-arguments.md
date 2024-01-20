---
title:                "Czytanie argumentów linii poleceń"
html_title:           "Bash: Czytanie argumentów linii poleceń"
simple_title:         "Czytanie argumentów linii poleceń"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/powershell/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Co i dlaczego?

Czytanie argumentów z linii komend pozwala na interakcyjne sterowanie programem poprzez wprowadzanie danych bezpośrednio podczas uruchamiania skryptu. Programiści wykorzystują tę technikę, aby zwiększyć elastyczność i składność ich skryptów.

## Jak to zrobić:

Przykład - Tworzymy skrypt, który wyświetla argumenty, które są przekazywane podczas uruchamiania:

```PowerShell
param (
  [string]$arg1,
  [string]$arg2
)

Write-Output "Pierwszy argument: $arg1"
Write-Output "Drugi argument: $arg2"
```

Wynik - Uruchomić skrypt z linii komend za pomocą argumentów `Hello` and `World` :

```PowerShell
.\yourscript.ps1 -arg1 Hello -arg2 World
Pierwszy argument: Hello
Drugi argument: World
```

## Wgłębianie się:

1. Historyczny kontekst: Argumenty linii komend stosowane są od dawna w systemach typu UNIX i w językach programowania takich jak C i Perl. PowerShell, będący późniejszym dodatkiem do rodziny języków skryptowych, dziedziczy i rozbudowuje tę funkcję.

2. Alternatywy: Inne sposoby obsługi argumentów to zastosowanie `$args` lub `[CmdletBinding()]`, które dają większą kontrolę i elastyczność.

3. Detale implementacji: Możemy obsługiwać różne typy argumentów m.in. stringi, liczby, tablice i obiekty. 

## Zobacz też:

- [Artykuł o obsłudze argumentów w PowerShell](https://adamtheautomator.com/powershell-parameters/)
- [Dokumentacja Microsoft o $args](https://docs.microsoft.com/pl-pl/powershell/module/microsoft.powershell.core/about/about_automatic_variables?view=powershell-7.1#args)