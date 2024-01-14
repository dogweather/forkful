---
title:    "Elm: Sprawdzanie czy istnieje ścieżka katalogu"
keywords: ["Elm"]
---

{{< edit_this_page >}}

# Dlaczego

Sprawdzanie, czy istnieje katalog, jest bardzo ważne w programowaniu, ponieważ umożliwia nam wykonywanie różnych operacji na plikach i folderach na naszym systemie. Może to być przydatne, na przykład, gdy tworzymy aplikację typu menedżer plików lub gdy chcemy zabezpieczyć nasz kod przed błędami wynikającymi z braku istniejącego katalogu.

# Jak to zrobić

Sprawdzanie czy istnieje katalog w Elm jest łatwe i wymaga użycia funkcji `Dir.doesExist`, która sprawdza, czy dany katalog istnieje i zwraca odpowiedni wynik w postaci typu `Maybe Bool`. 

```elm
Dir.doesExist "ścieżka/do/katalogu"
    |> Task.attempt HandleResult
```

W powyższym przykładzie używamy funkcji `Dir.doesExist` do sprawdzenia, czy podana ścieżka do katalogu istnieje. Następnie, używając funkcji `Task.attempt`, wykonujemy asynchroniczne zadanie i obsługujemy jego wynik w funkcji `HandleResult`. 

```elm
type alias Model =
    { isDirectoryExisting : Maybe Bool
    , error : Maybe Error
    }

HandleResult result =
    case result of
        Ok isExisting ->
            { model | isDirectoryExisting = Just isExisting }

        Err error ->
            { model | error = Just error }
```

Funkcja `HandleResult` odpowiada za obsługę wyników zadania `Dir.doesExist`. W przypadku sukcesu przekazujemy wynik do modelu, a w przypadku wystąpienia błędu, przypisujemy go do pola `error` w modelu.

# Dogłębna analiza

W Elm, sprawdzenie czy istnieje katalog jest wykonywane za pomocą wywołania funkcji `Dir.doesExist` z biblioteki `elm/file`. Ta funkcja wywołuje odpowiednie API systemowe (na przykład `stat()` w systemie Unix) w celu sprawdzenia, czy dany katalog istnieje.

Warto również zwrócić uwagę, że funkcja `Dir.doesExist` jest funkcją asynchroniczną, co oznacza, że zwróci wartość w postaci typu `Task` zamiast wartości bezpośrednio. Dzięki temu, aplikacja nie zostanie zablokowana podczas oczekiwania na wynik.

## Zobacz również

* [Dokumentacja funkcji `Dir.doesExist`](https://package.elm-lang.org/packages/elm/file/latest/Dir#doesExist)
* [Funkcja `Task.attempt`](https://package.elm-lang.org/packages/elm/core/latest/Task#attempt)