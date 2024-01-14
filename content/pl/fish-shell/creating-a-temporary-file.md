---
title:                "Fish Shell: Tworzenie pliku tymczasowego"
simple_title:         "Tworzenie pliku tymczasowego"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/fish-shell/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Dlaczego

Tworzenie tymczasowych plików jest powszechną praktyką w programowaniu. Często jest to niezbędne, gdy potrzebujemy tymczasowych danych lub plików, które nie są potrzebne po zakończeniu działania programu.

## Jak to zrobić

Tworzenie tymczasowych plików jest bardzo proste w Fish Shell. Możemy użyć wbudowanej funkcji `mktemp`, która automatycznie generuje unikalną nazwę dla naszego pliku tymczasowego.

```Fish Shell
set tempfile (mktemp)
echo "To jest tymczasowy plik" > $tempfile
cat $tempfile
```

W powyższym przykładzie tworzymy zmienną `tempfile` i przypisujemy jej wartość zwróconą przez funkcję `mktemp`. Następnie do naszego pliku tymczasowego zapisujemy przykładowy tekst i wyświetlamy go na konsoli za pomocą komendy `cat`.

Możemy również ustalić konkretną nazwę dla naszego pliku tymczasowego, jeśli chcemy. W tym celu możemy wykorzystać opcję `-p` funkcji `mktemp`.

```Fish Shell
set tempfile (mktemp -p data)
echo "To jest tymczasowy plik w folderze data" > $tempfile
cat $tempfile
```

W ten sposób plik zostanie automatycznie stworzony w określonym przez nas folderze.

## Przeanalizujmy to dokładniej

Tworząc tymczasowe pliki w ten sposób, musimy pamiętać, że zostaną one automatycznie usunięte po zakończeniu działania programu. Jeśli chcemy zachować plik, musimy go przenieść lub skopiować w innej części skryptu.

Istnieją również inne metody tworzenia tymczasowych plików w Fish Shell, na przykład za pomocą funkcji `mktemp -s` lub `mktemp -t`. Możemy również użyć polecenia `touch`, aby utworzyć pusty plik tymczasowy.

## Zobacz również

- Dokumentacja funkcji `mktemp` w Fish Shell: https://fishshell.com/docs/current/cmds/mktemp.html
- Wprowadzenie do podstaw programowania w Fish Shell: https://fishshell.com/docs/current/tutorial.html
- Przykłady i dalsze zastosowania tworzenia tymczasowych plików w Fish Shell: https://www.davidbegin.com/create-temporary-files-and-directories-in-fish-shell/