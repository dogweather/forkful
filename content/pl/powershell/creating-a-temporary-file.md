---
title:                "Tworzenie tymczasowego pliku"
html_title:           "PowerShell: Tworzenie tymczasowego pliku"
simple_title:         "Tworzenie tymczasowego pliku"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/powershell/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

Title: Jak utworzyć plik tymczasowy w PowerShell?

## Czym jest i dlaczego to robimy?
Tworzenie tymczasowego pliku jest bardzo popularną czynnością w programowaniu, polegającą na utworzeniu pliku, który jest używany tylko przez krótki okres czasu, a następnie usuwany. Programiści często tworzą tymczasowe pliki, aby przeprowadzić operacje na danych lub przechowywać pewne informacje przez chwilę. Pozwala to uniknąć zaciemnienia głównego pliku lub bazy danych.

## Jak to zrobić:
```PowerShell
# Utworzenie nowego tymczasowego pliku za pomocą New-TemporaryFile
$tempFile = New-TemporaryFile

# Wyświetlenie ścieżki do utworzonego pliku
$tempFile.FullName

# Przetworzenie pliku lub zapisanie informacji do niego
# ...

# Usunięcie tymczasowego pliku po zakończeniu operacji
Remove-Item $tempFile.FullName
```

## Głębsza analiza:
Tworzenie tymczasowych plików jest praktykowane już od dłuższego czasu w programowaniu. Wcześniej programiści musieli sami zarządzać tymczasowymi plikami, a następnie usuwać je ręcznie po zakończeniu operacji. Dzięki PowerShell możemy to zrobić szybko i wygodnie, korzystając z wbudowanej funkcji New-TemporaryFile.

Alternatywną metodą jest użycie np. zmiennej tymczasowej w pamięci RAM lub dodatkowej bazy danych, jednakże tworzenie i usuwanie tymczasowego pliku jest wygodniejsze i może przynieść lepsze wyniki wydajnościowe.

Warto również wspomnieć, że domyślnie plik tymczasowy jest usuwany automatycznie po zakończeniu sesji PowerShell. Jeśli jednak chcemy zachować go na stałe, możemy zmienić to zachowanie, dodając parametr -Persist do funkcji New-TemporaryFile.

## Zobacz także:
- [Dokumentacja funkcji New-TemporaryFile w PowerShell](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.utility/new-temporaryfile)
- [Blog Microsoftu na temat tworzenia i usuwania tymczasowych plików w PowerShell](https://devblogs.microsoft.com/scripting/using-temporary-files-in-powershell/)