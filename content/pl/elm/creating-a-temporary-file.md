---
title:                "Elm: Tworzenie pliku tymczasowego"
simple_title:         "Tworzenie pliku tymczasowego"
programming_language: "Elm"
category:             "Elm"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/elm/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Dlaczego?

Tworzenie tymczasowych plików jest ważnym aspektem programowania, ponieważ umożliwia nam tymczasowe przechowywanie danych lub wyników obliczeń. Jest to szczególnie przydatne w przypadku tworzenia aplikacji internetowych, gdzie często potrzebujemy zapisać dane do pliku, ale nie chcemy ich trwale przechowywać na serwerze. Elm oferuje prosty sposób na tworzenie tymczasowych plików, co ułatwia nam pracę z danymi w naszych aplikacjach. 

## Jak to zrobić?

Aby stworzyć tymczasowy plik w Elm, musimy użyć funkcji `File.temp` z modułu `File`. Funkcja ta przyjmuje dwa argumenty: nazwę pliku oraz jego zawartość. W poniższym przykładzie stworzymy tymczasowy plik o nazwie "temp.txt" zawierający tekst "To jest przykładowa zawartość pliku":

```
Elm File.temp "temp.txt" "To jest przykładowa zawartość pliku"
```

Możemy też użyć funkcji `File.write` do zapisania danych do utworzonego przez nas pliku:

```
Elm File.write "temp.txt" "Nowa zawartość pliku"
```

Aby odczytać zawartość tymczasowego pliku, możemy użyć funkcji `File.read`:

```
Elm File.read "temp.txt"
```

W wyniku otrzymamy tekst "Nowa zawartość pliku". Po zakończeniu pracy z plikiem, możemy go usunąć za pomocą funkcji `File.delete`:

```
Elm File.delete "temp.txt"
```

## W głębi programowania

Tworzenie tymczasowych plików w Elm bazuje na funkcjonalności modułu `File`. Warto zdawać sobie sprawę z kilku istotnych elementów przy korzystaniu z tej funkcjonalności:

1. Pliki tymczasowe są usuwane automatycznie po zakończeniu działania naszej aplikacji.
2. Domyślnie pliki tymczasowe są zapisywane w tej samej lokalizacji, co aplikacja.
3. Możemy określić inną lokalizację poprzez podanie pełnej ścieżki dostępu w nazwie pliku.

Tworzenie tymczasowych plików jest bardzo przydatną umiejętnością w programowaniu, a dzięki prostym funkcjom dostępnym w Elm, możemy to zrobić w łatwy i bezpieczny sposób.

## Zobacz także

* [Dokumentacja Elm o tymczasowych plikach](https://guide.elm-lang.org/io/files.html)
* [Tutorial dotyczący obsługi plików w Elm](https://dev.to/rtfeldman/elm-files-without-ports-a-walkthrough-of-how-to-talk-to-external-files-1kja)