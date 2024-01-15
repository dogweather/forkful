---
title:                "Odczytywanie pliku tekstowego"
html_title:           "Elm: Odczytywanie pliku tekstowego"
simple_title:         "Odczytywanie pliku tekstowego"
programming_language: "Elm"
category:             "Elm"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/elm/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Dlaczego

Czy kiedykolwiek chcieliście przeczytać tekstowy plik w swoim programie w Elm? Może potrzebujecie przetworzyć do danych lub wykonać inne operacje. W tym artykule dowiesz się, jak w łatwy i przyjemny sposób czytać pliki tekstowe w programie Elm.

## Jak To Zrobić

Aby przeczytać plik tekstowy w programie Elm, musimy użyć modułu `File` i jego funkcji `readString`. Poniżej przedstawiamy prosty przykład kodu, który otworzy i wyświetli zawartość pliku `input.txt`.

```
Elm
import File
import Html exposing (text)

main =
  File.readString "input.txt" []
    |> Result.map (\contents -> text contents)
    |> Result.withDefault (text "Nie udało się otworzyć pliku.")
```

Wywołujemy funkcję `readString`, przekazując jej nazwę pliku oraz pustą listę opcji. Następnie używamy funkcji `Result.map` do przetworzenia wyniku odczytania pliku na element `HTML` zawierający zawartość pliku. Ważne jest również użycie funkcji `Result.withDefault` w razie niepowodzenia odczytu pliku. Ostatecznie, wyświetlamy zawartość pliku na stronie za pomocą funkcji `text` z modułu `Html`.

## Deep Dive

Funkcja `readString` zwraca wartość typu `Result`, która może przyjąć dwa warianty: `Ok` lub `Err`. W przypadku sukcesu, wariant `Ok` zawiera zawartość pliku w postaci `String`, a w przypadku błędu, wariant `Err` zawiera informację o błędzie. Aby uzyskać więcej informacji o błędzie, można użyć funkcji `File.getError`.

Kodowanie plików jest również ważnym zagadnieniem podczas czytania plików tekstowych w programie Elm. Funkcja `readString` przyjmuje opcjonalną listę opcji, w której można ustawić żądane kodowanie za pomocą pary klucz-wartość, np. ` ("encoding", "UTF-8")`. Istnieje wiele dostępnych kodowań, takich jak `UTF-8`, `UTF-16` czy `ISO-8859-1`. Domyślne kodowanie różni się w zależności od środowiska uruchomieniowego, dlatego zawsze warto jawnie ustawić preferowane kodowanie.

## Zobacz też

Poniżej znajdują się kilka linków do innych przydatnych artykułów na temat programowania w Elm:

- [Elm - oficjalna strona języka](https://elm-lang.org/)
- [Dokumentacja modułu File w Elm](https://package.elm-lang.org/packages/elm/file/latest/File)
- [Poradnik: Wprowadzenie do programowania w Elm](https://guide.elm-lang.org/)
- [Przykładowe projekty w Elm](https://github.com/mdgriffith/elm-nuts-and-bolts)