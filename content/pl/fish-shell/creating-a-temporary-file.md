---
title:                "Tworzenie tymczasowego pliku"
html_title:           "Fish Shell: Tworzenie tymczasowego pliku"
simple_title:         "Tworzenie tymczasowego pliku"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/fish-shell/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Dlaczego

Tworzenie plików tymczasowych jest niezbędnym elementem programowania w Fish Shell. Często potrzebujemy tymczasowego pliku do przechowywania danych lub wyników działania programu. Tworzenie tymczasowych plików jest szybkie, łatwe i wygodne dzięki wbudowanym funkcjom Fish Shell.

## Jak to zrobić

Aby utworzyć tymczasowy plik w Fish Shell, możemy skorzystać z funkcji `mktemp`, która automatycznie tworzy unikalny plik i zwraca jego ścieżkę. Przykładowy kod wyglądałby następująco:

```Fish Shell
set temp_file (mktemp)
touch $temp_file
echo "To jest przykładowa treść" > $temp_file
cat $temp_file
```

Powyższy kod utworzy tymczasowy plik, doda do niego tekst, a następnie wyświetli jego zawartość. Dzięki użyciu funkcji `mktemp`, nie musimy samodzielnie wymyślać unikalnych nazw plików, co ułatwia nam zadanie.

Możemy również utworzyć tymczasowy katalog przy pomocy funkcji `mktemp -d`, a także ustalić prefix lub suffix dla nazwy tymczasowego pliku lub katalogu, na przykład `mktemp -p prefix_ -s _suffix`. Więcej informacji na temat funkcji `mktemp` możemy znaleźć w jej dokumentacji lub wpisując w terminalu komendę `help mktemp`.

## Deep Dive

Tworzenie tymczasowego pliku w Fish Shell odbywa się poprzez utworzenie gałęzi wirtualnego systemu plików (VFS), zwanego `tmpfs`. Jest to system plików, który istnieje tylko w pamięci RAM i jest automatycznie usuwany po zakończeniu sesji użytkownika. Dzięki temu pliki tymczasowe są szybkie i nie obciążają dysku twardego.

Pliki tymczasowe są szczególnie przydatne w przypadku, gdy chcemy przetestować lub wykorzystać jakąś funkcję, ale nie chcemy podpisywać się pod zmianami w pliku źródłowym. Dzięki temu możemy bezpiecznie pracować na kopii, która zostanie automatycznie usunięta po zakończeniu sesji.

## Zobacz także

- Dokumentacja funkcji `mktemp`
- Poradnik tworzenia tymczasowych plików w Fish Shell na blogu Fisherman.pl
- Temat na forum Fish Shell dotyczący korzystania z plików tymczasowych