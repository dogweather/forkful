---
date: 2024-01-20 18:04:20.682126-07:00
description: "How to: (Jak to zrobi\u0107:) Tworzenie nowego projektu w PowerShellu\
  \ zaczyna si\u0119 od struktury katalog\xF3w i plik\xF3w. U\u017Cyj `New-Item` do\
  \ tworzenia\u2026"
lastmod: '2024-04-05T21:53:37.055353-06:00'
model: gpt-4-1106-preview
summary: "(Jak to zrobi\u0107:) Tworzenie nowego projektu w PowerShellu zaczyna si\u0119\
  \ od struktury katalog\xF3w i plik\xF3w."
title: Rozpoczynanie nowego projektu
weight: 1
---

## How to: (Jak to zrobić:)
Tworzenie nowego projektu w PowerShellu zaczyna się od struktury katalogów i plików. Użyj `New-Item` do tworzenia katalogów/projektów, a `Set-Location` do zmiany aktywnego katalogu.

```PowerShell
# Tworzenie katalogu dla nowego projektu
New-Item -ItemType Directory -Path 'C:\MojeProjekty\NowyProjekt'

# Przechodzimy do nowoutworzonego katalogu
Set-Location -Path 'C:\MojeProjekty\NowyProjekt'

# Utworzenie pliku .ps1 dla skryptów
New-Item -ItemType File -Path '.\Start.ps1'

# Sprawdzenie zawartości katalogu projektu
Get-ChildItem -Path .
```

Sample output (przykładowe wyniki):
```
    Directory: C:\MojeProjekty\NowyProjekt

Mode                 LastWriteTime         Length Name
----                 -------------         ------ ----
-a----          12/4/2023   3:30 PM              0 Start.ps1
```

## Deep Dive (Dogłębna analiza):
PowerShell, od czasu wersji 5.0 i wprowadzenia PowerShellGet, daje deweloperom narzędzia podobne do tych znanych z innych środowisk programistycznych. Ułatwia to zarządzanie zależnościami (np. moduły) i wdrażanie przy użyciu `New-ModuleManifest` lub `New-ScriptFileInfo`. Alternatywą są użycie IDE jak Visual Studio Code z właściwymi dodatkami poprawiającymi workflow.

Dzięki wersji PowerShell Core (cross-platform), projektowanie modułów powinno być robione z myślą o kompatybilności między platformami. Historia PowerShella jest unikatowa, jako fork projektu Microsoftu, który był zainspirowany przez Unixowe powłoki jak Bash. Nowy projekt w PowerShellu może być prostym skryptem, aż po zestaw osadzonych funkcji jako moduł.

## See Also (Zobacz również):
- [Dokumentacja PowerShell](https://docs.microsoft.com/powershell/)
- [Narzędzia w Visual Studio Code dla PowerShell](https://code.visualstudio.com/docs/languages/powershell)
- [PowerShell Gallery](https://www.powershellgallery.com/)
