---
title:                "Sprawdzanie czy istnieje katalog"
html_title:           "PowerShell: Sprawdzanie czy istnieje katalog"
simple_title:         "Sprawdzanie czy istnieje katalog"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/powershell/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Co i dlaczego?

Sprawdzanie, czy istnieje katalog, to proces, w którym programista określa, czy wskazany folder istnieje na komputerze lub serwerze. Jest to ważna część programowania, ponieważ pozwala na wykonywanie odpowiednich akcji w zależności od tego, czy katalog istnieje czy nie.

## Jak to zrobić:

```PowerShell
if (Test-Path -Path "C:\Users\Example\Documents") {
    Write-Host "Katalog istnieje."
}
else {
    New-Item -Path "C:\Users\Example\Documents" -ItemType Directory
    Write-Host "Utworzono nowy katalog."
}
```
**Output:** Katalog istnieje.

```PowerShell
if (Test-Path -Path "C:\Users\Example\Pictures") {
    Remove-Item -Path "C:\Users\Example\Pictures" -Recurse
    Write-Host "Katalog usunięty."
}
else {
    Write-Host "Katalog nie istnieje."
}
```
**Output:** Katalog nie istnieje.

## Głębokie zanurzenie:

Sprawdzanie istnienia katalogu może być stosowane w wielu różnych sytuacjach programistycznych. Jest to szczególnie przydatne, gdy tworzymy skrypty automatyzujące kopiowanie lub usuwanie plików w konkretnym katalogu. Wcześniej, aby wykonać to zadanie, musieliśmy ręcznie sprawdzać, czy katalog istnieje, a następnie podejmować odpowiednie działania. Dzięki funkcji Test-Path w PowerShell, cały ten proces stał się znacznie prostszy i bardziej wydajny.

Alternatywnym sposobem sprawdzania, czy istnieje katalog, jest użycie metody "Exists" na obiekcie Directory w języku C#. Jednak, w przypadku programowania w PowerShell, funkcja Test-Path jest bardziej wydajna i łatwiejsza do użycia.

Kiedy funkcja Test-Path jest wykonywana, sprawdza ona, czy określona ścieżka jest prawidłowa i czy istnieje na komputerze lub serwerze. Może być również używana do sprawdzenia istnienia plików lub rejestrów systemu.

## Zobacz również:

Jeśli chcesz dowiedzieć się więcej o funkcji Test-Path w PowerShell, możesz sprawdzić oficjalną dokumentację na stronie internetowej Microsoft: https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.management/test-path

Możesz również sprawdzić inne źródła, które pomogą Ci lepiej zrozumieć, jak i kiedy używać funkcji Test-Path w swoich skryptach PowerShell.