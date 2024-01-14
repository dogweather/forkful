---
title:    "Gleam: Odczyt pliku tekstowego"
keywords: ["Gleam"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pl/gleam/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Dlaczego

Istnieje wiele powodów, dla których warto nauczyć się czytać pliki tekstowe w języku programowania Gleam. Po pierwsze, jest to często wykorzystywane w różnych projektach i może okazać się niezbędne dla Twojej przyszłej pracy. Po drugie, umiejętność czytania plików tekstowych może pomóc w efektywnym przetwarzaniu danych i automatyzacji różnych zadań.

## Jak to zrobić

Aby czytać pliki tekstowe w języku Gleam, należy użyć funkcji `File.read`. Przykładowy kod znajduje się poniżej:

```Gleam
// Otwarcie pliku tekstowego do odczytu
let file = File.open("tekst.txt", "r")

// Wczytanie całego pliku jako jednego napisu
let content = File.read(file, "all")

// Wyświetlenie zawartości pliku tekstowego
IO.println(content)

// Zamknięcie pliku
File.close(file)
```

W powyższym przykładzie w pierwszej linijce otwieramy plik tekstowy "tekst.txt" w trybie tylko do odczytu. Następnie wywołujemy funkcję `File.read`, która zwraca zawartość pliku jako napis. Ostatecznie zamykamy plik, aby zwolnić zasoby systemowe.

Przykładowy wynik działania powyższego kodu może wyglądać następująco:

```
To jest zawartość pliku tekst.txt
```

## Głębsze zagłębienie

Wczytywanie plików tekstowych nie jest jedynym sposobem na wykorzystanie funkcji `File.read`. Funkcja ta może być również użyta do wczytania wybranego fragmentu pliku, np. 10 pierwszych linii lub 100 ostatnich. Ponadto, można również zastosować różne tryby otwierania pliku, np. do zapisu lub do odczytu i zapisu na raz.

Właściwe wykorzystanie funkcji `File.read` może znacznie ułatwić pracę z plikami tekstowymi w języku Gleam.

## Zobacz również

- Dokumentacja Gleam: https://gleam.run/documentation/
- Wprowadzenie do czytania i pisania plików tekstowych w języku Gleam: https://gleam.run/articles/file-reader-writer
- Przykładowy projekt wykorzystujący funkcję `File.read`: https://github.com/gleam-lang/example-file-reader