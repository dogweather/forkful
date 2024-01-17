---
title:                "Odczytywanie pliku tekstowego"
html_title:           "PowerShell: Odczytywanie pliku tekstowego"
simple_title:         "Odczytywanie pliku tekstowego"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/powershell/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Co & dlaczego?
Czy kiedykolwiek zastanawiałeś się, jak programiści odczytują zawartość plików tekstowych? To nic innego jak umiejętność odczytywania linia po linii tekstu z pliku, aby móc przetworzyć te informacje i wykorzystać je w swoim kodzie. Jest to ważna umiejętność dla każdego programisty, ponieważ pliki tekstowe są jednym z podstawowych sposobów przechowywania danych w systemach operacyjnych.

## Jak to zrobić:
```PowerShell
$plik = Get-Content -Path "sciezka/do/pliku.txt" # przypisanie zawartości pliku do zmiennej
$plik # wyświetlenie zawartości pliku w konsoli
```

Kod powyżej pokazuje, że w celu odczytania pliku tekstowego w PowerShell, możemy użyć cmdletu `Get-Content` i podać ścieżkę do pliku jako argument. Następnie przypisujemy odczytaną zawartość do zmiennej i możemy z niej korzystać w swoim kodzie. Możemy również wyświetlić zawartość w konsoli, używając nazwy zmiennej.

## Deep Dive:
Odczytywanie plików tekstowych jest ważną umiejętnością od czasów powstania komputerów. Przed wynalezieniem graficznego interfejsu użytkownika, wszystkie dane były zapisywane i przetwarzane jako tekst, dlatego umiejętność odczytywania plików tekstowych była niezbędna dla każdego programisty.

Alternatywnym sposobem odczytywania plików tekstowych w PowerShell jest użycie metody `ReadAllText` klasy `System.IO.File`. Może to być przydatne, jeśli chcemy odczytać cały plik na raz i nie potrzebujemy przetwarzać go linia po linii.

Implementacyjne szczegóły odczytywania pliku tekstowego w PowerShell różnią się w zależności od systemu operacyjnego, na którym jest uruchomiony skrypt. Na przykład, w systemie Windows, standardowe znaki końca linii są reprezentowane jako `CR LF` (carriage return i line feed), podczas gdy w systemie Linux jako `LF` (line feed).

## Zobacz również:
- Dokumentacja Microsoft dotycząca cmdletu [Get-Content](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.management/get-content?view=powershell-7)
- Tutorial na temat [pracy z plikami tekstowymi w PowerShell](https://www.pluralsight.com/guides/working-with-file-content)
- Książka "Learn Windows PowerShell in a Month of Lunches" autorstwa Dona Jonesa.