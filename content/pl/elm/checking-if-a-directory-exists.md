---
title:    "Elm: Sprawdzanie istnienia folderu"
keywords: ["Elm"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pl/elm/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Dlaczego
Jeśli jesteś programistą Elm i często pracujesz z plikami i katalogami, możesz zainteresować się sprawdzeniem, czy dany katalog istnieje. Jest to ważna umiejętność, ponieważ pozwala na uniknięcie błędów w kodzie i zapewnia bezpieczeństwo w obsłudze plików.

## Jak to zrobić
Aby sprawdzić istnienie katalogu, możesz skorzystać z funkcji `dirExists`, która sprawdza, czy podana ścieżka wskazuje na istniejący katalog. Przykład kodu wyglądałby następująco:

```Elm
dirExists : FilePath -> Task x Bool
```

Jeśli `Task` zwróci wartość `True`, oznacza to, że dany katalog istnieje. Możesz również przetestować to na przykładowym kodzie i wyświetlić wynik za pomocą poniższego kodu:

```Elm
dirExists "/home/user/Documents"
    |> andThen (\result -> Html.text (toString result))
```

W tym przypadku wartość `True` zostanie wyświetlona na stronie. W przypadku, gdy ścieżka będzie nieprawidłowa lub katalog nie istnieje, zostanie wyświetlona wartość `False`.

## Głębszy zanurzenie
Funkcja `dirExists` jest implementowana przy użyciu biblioteki [elm/file](https://package.elm-lang.org/packages/elm/file/latest/) i wykorzystuje moduł `FileSystem`. Istnieje jednak inny sposób na sprawdzenie istnienia katalogu, a mianowicie przy użyciu funkcji `list` oraz komparatora `isDir`, która zwraca tylko katalogi znajdujące się w danym folderze.

## Zobacz również
- [Dokumentacja elm/file](https://package.elm-lang.org/packages/elm/file/latest/)
- [Przykładowy kod wideo elm/file](https://www.youtube.com/watch?v=2WNOFDjZgus)