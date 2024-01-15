---
title:                "Sprawdzanie istnienia katalogu"
html_title:           "Elm: Sprawdzanie istnienia katalogu"
simple_title:         "Sprawdzanie istnienia katalogu"
programming_language: "Elm"
category:             "Elm"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/elm/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Dlaczego

Sprawdzanie istnienia katalogu może być przydatne w wielu przypadkach, na przykład podczas tworzenia programów, które wymagają dostępu do plików przechowywanych w konkretnym katalogu. W ten sposób możemy upewnić się, że nasz program będzie działał poprawnie i nie będzie informował nas o nieprawidłowej ścieżce.

## Jak to zrobić

Sprawdzanie istnienia katalogu jest prostym procesem w języku Elm. Wystarczy użyć funkcji `directoryExists` z biblioteki `elm/file` i podać jej ścieżkę do katalogu jako argument. Jeśli katalog istnieje, funkcja zwróci wartość `True`, w przeciwnym przypadku zwróci `False`. Poniżej znajduje się przykładowy kod, który wykorzystuje tę funkcję:

```elm
import File exposing (directoryExists)
import Task exposing (attempt)

directoryPath = "moj/katalog"

checkDirectory = 
    attempt
        always ()
        (\ _ -> directoryExists directoryPath)
```

W tym przykładzie używa się również funkcji `Task.attempt`, która pomaga obsłużyć ewentualne błędy przy wywoływaniu funkcji `directoryExists`.

## Wnikliwa analiza

Sprawdzanie istnienia katalogu w języku Elm jest możliwe dzięki wykorzystaniu interfejsu JavaScriptowych funkcji API przeglądarki. Funkcja `directoryExists` jest opakowanym interfejsem dla funkcji `DirectoryHandle.query()` z API `Directory System Access`, która pozwala na sprawdzenie istnienia katalogu w określonej ścieżce. W przypadku braku dostępu do systemu plików, funkcja zwróci komunikat błędu.

## Zobacz także

- [Dokumentacja funkcji directoryExists](https://package.elm-lang.org/packages/elm/file/latest/File#directoryExists)
- [Dokumentacja interfejsu API Directory System Access](https://developer.mozilla.org/en-US/docs/Web/API/Directory_System_Access_API)
- [Przykład wykorzystania funkcji directoryExists w projekcie Elm](https://guide.elm-lang.org/effect_managers/file_system.html#checking-directory-existence)