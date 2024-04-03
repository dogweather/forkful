---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:29:30.659097-07:00
description: "Tworzenie pliku tekstowego w PowerShell polega na tworzeniu i manipulowaniu\
  \ plikami opartymi na tek\u015Bcie, co jest fundamentaln\u0105 operacj\u0105 dla\
  \ logowania,\u2026"
lastmod: '2024-03-13T22:44:35.648181-06:00'
model: gpt-4-0125-preview
summary: "Tworzenie pliku tekstowego w PowerShell polega na tworzeniu i manipulowaniu\
  \ plikami opartymi na tek\u015Bcie, co jest fundamentaln\u0105 operacj\u0105 dla\
  \ logowania, przechowywania danych i pisania skrypt\xF3w konfiguracji."
title: Pisanie pliku tekstowego
weight: 24
---

## Co i dlaczego?
Tworzenie pliku tekstowego w PowerShell polega na tworzeniu i manipulowaniu plikami opartymi na tekście, co jest fundamentalną operacją dla logowania, przechowywania danych i pisania skryptów konfiguracji. Programiści wykorzystują to do automatyzacji zadań systemowych, analizy danych oraz integracji z innymi aplikacjami lub skryptami.

## Jak:
PowerShell zapewnia proste polecenia cmdlet do obsługi plików. Głównie używa się cmdletu `Out-File` oraz operatorów przekierowania do tego celu. Oto przykłady ilustrujące jak zapisać tekst do plików w różnych scenariuszach:

**Podstawowe tworzenie pliku tekstowego:**

Aby stworzyć plik tekstowy i zapisać do niego prosty ciąg tekstowy, możesz użyć:

```powershell
"Hello, World!" | Out-File -FilePath .\example.txt
```

Lub równoważnie z operatorem przekierowania:

```powershell
"Hello, World!" > .\example.txt
```

**Dopisywanie tekstu do istniejącego pliku:**

Jeśli chcesz dodać tekst na końcu istniejącego pliku bez nadpisywania go:

```powershell
"Another line." | Out-File -FilePath .\example.txt -Append
```

Lub używając operatora przekierowania do dopisywania:

```powershell
"Another line." >> .\example.txt
```

**Zapisywanie wielu linii:**

Do zapisywania wielu linii możesz użyć tablicy ciągów tekstowych:

```powershell
$lines = "Line 1", "Line 2", "Line 3"
$lines | Out-File -FilePath .\multilines.txt
```

**Określanie kodowania:**

Aby określić konkretne kodowanie tekstu, użyj parametru `-Encoding`:

```powershell
"Text with UTF8 Encoding" | Out-File -FilePath .\utfexample.txt -Encoding UTF8
```

**Użycie bibliotek firm trzecich:**

Chociaż wbudowane polecenia cmdlet w PowerShell wystarczają do podstawowych operacji na plikach, bardziej skomplikowane zadania mogą korzystać z modułów firm trzecich, takich jak `PowershellGet`, czy narzędzi jak `SED` i `AWK` przeniesionych dla systemu Windows. Jednak do czystego zapisywania pliku tekstowego mogą one być niepotrzebne i ogólnie rzecz biorąc, nie są wymagane:

```powershell
# Zakładając bardziej skomplikowany scenariusz uzasadniający użycie zewnętrznej biblioteki
# Install-Module -Name SomeComplexLibrary
# Import-Module -Name SomeComplexLibrary
# Tutaj bardziej skomplikowane operacje
```

_Uwaga: Zawsze zastanów się, czy złożoność dodania zależności od strony trzeciej jest uzasadniona dla Twoich potrzeb._

**Przykładowe Wyjście:**

Po wykonaniu podstawowej komendy tworzenia pliku, sprawdzając zawartość `example.txt`, pokazuje:

```plaintext
Hello, World!
```

Po dopisaniu tekstu i ponownym sprawdzeniu `example.txt`:

```plaintext
Hello, World!
Another line.
```
