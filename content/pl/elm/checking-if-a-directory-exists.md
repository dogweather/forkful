---
title:                "Elm: Sprawdzanie, czy istnieje katalog."
simple_title:         "Sprawdzanie, czy istnieje katalog."
programming_language: "Elm"
category:             "Elm"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/elm/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Dlaczego warto sprawdzać istnienie katalogu?

Sprawdzanie, czy dany katalog istnieje, jest ważnym aspektem programowania w Elm. Jest to szczególnie przydatne w przypadku aplikacji internetowych, które często muszą odwoływać się do plików lub katalogów na serwerze. Sprawdzenie, czy katalog istnieje, pomaga uniknąć błędów związanych z dostępem do plików, a także zapewnia bezpieczeństwo aplikacji.

## Jak to zrobić?

Aby sprawdzić istnienie katalogu w Elm, należy użyć funkcji `Directory.exists` z modułu `Elm.Kernel.Directory`. Poniżej przedstawiono przykładowy kod, który demonstruje tę funkcję:

```
Elm.Kernel.Directory.exists "sciezka/do/katalogu" 
    |> Result.andThen (\exists -> 
        if exists then 
            "Katalog istnieje." |> Result.ok
        else
            "Katalog nie istnieje." |> Result.err)
    |> Result.withDefault "Wystąpił błąd."
```

Kod ten pozwala najpierw na sprawdzenie, czy katalog istnieje. Jeśli tak, zostanie zwrócony pozytywny wynik, w przeciwnym wypadku zostanie zwrócony błąd. Należy pamiętać, że funkcja `exists` zwraca wartość typu `Result`, więc musimy obsłużyć zarówno przypadki sukcesu, jak i błędu.

## Wnikliwa analiza

Sprawdzanie istnienia katalogu może być także przydatne w sytuacjach, gdy użytkownik aplikacji musi wybrać konkretny katalog, w którym ma zostać zapisany plik lub dane. W takim przypadku można wyświetlić użytkownikowi listę dostępnych katalogów i sprawdzić, czy wybrany przez niego istnieje.

## Zobacz także

* Dokumentacja dotycząca funkcji `Directory.exists` w oficjalnym podręczniku Elm (https://guide.elm-lang.org/interop/javascript.html#calling-a-function)
* Przykładowy kod wykorzystujący `Directory.exists` na stronie GitHub (https://github.com/some-user/some-repo)
* Dyskusja na forum Elm Dev o wykorzystaniu `Directory.exists` w aplikacji webowej (https://discourse.elm-lang.org/t/check-if-directory-directory-exists/1234)