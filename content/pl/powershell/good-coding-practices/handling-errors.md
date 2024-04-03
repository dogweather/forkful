---
date: 2024-01-26 00:56:36.064244-07:00
description: "Jak to zrobi\u0107: ."
lastmod: '2024-03-13T22:44:35.637286-06:00'
model: gpt-4-1106-preview
summary: .
title: "Obs\u0142uga b\u0142\u0119d\xF3w"
weight: 16
---

## Jak to zrobić:
```PowerShell
# Podstawowy blok Try-Catch do obsługi wyjątków
try {
    # Kod, który może wywołać błąd
    $result = 1 / 0
} catch {
    # Co zrobić, gdy wystąpi błąd
    Write-Host "Ups, wystąpił błąd: $_"
}

# Wypisywanie własnego komunikatu o błędzie
try {
    Get-Item "nonexistentfile.txt" -ErrorAction Stop
} catch {
    Write-Host "Nie można znaleźć pliku."
}

# Używanie zmiennej $Error do zbadania ostatniego błędu
```

## Pogłębiona wiedza
PowerShell przeszedł długą drogę od czasu swoich początków jako Monad. Obsługa błędów stała się z czasem bardziej solidna, oferując funkcje podobne do tych z innych języków programowania. Składnia `try-catch-finally` to jeden z przykładów krzyżowania się z językami takimi jak C#. Przed jej wprowadzeniem, skrypciarze polegali głównie na sprawdzaniu warunków i używaniu automatycznej zmiennej `$Error`.

PowerShell ma także dwa główne typy błędów: terminujące i nieteminujące. Błędy terminujące zatrzymają skrypt, chyba że zostaną przechwycone w bloku `try-catch`, podczas gdy błędy nieteminujące nie zrobią tego, chyba że określisz `-ErrorAction Stop`. To rozróżnienie jest kluczowe, ponieważ daje precyzyjną kontrolę nad obsługą błędów, decydując czy błąd rzeczywiście wymaga zatrzymania całego skryptu lub może być po prostu zalogowany i zignorowany.

Obsługa błędów w PowerShell umożliwia także użycie bloku `finally`, który wykona się niezależnie od wszystkiego - bez względu na to, czy wystąpił błąd, czy nie. Jest świetny do zadań związanych z oczyszczaniem.

Kiedy jesteś głęboko w okopach skryptów, możesz także obsługiwać konkretny typ wyjątków, co daje jeszcze większą kontrolę.

Alternatywnie, istnieje stara, dobra metoda - parametr `-ErrorVariable` do przechwytywania błędów bez rzucania wyjątkiem. A zmienna `$?` informuje Cię, czy ostatnia operacja zakończyła się sukcesem. Są to przydatne narzędzia, choć nieco mniej eleganckie niż solidny blok `try-catch`.

## Zobacz też
- [about_Try_Catch_Finally](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.core/about/about_try_catch_finally?view=powershell-7.2)
