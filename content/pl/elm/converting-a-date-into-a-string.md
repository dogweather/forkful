---
title:    "Elm: Konwersja daty na ciąg znaków"
keywords: ["Elm"]
---

{{< edit_this_page >}}

## Dlaczego 
W dzisiejszych czasach programowanie jest niezbędnym składnikiem wielu dziedzin, w tym także aplikacji internetowych. Jedną z kluczowych czynności w aplikacjach jest konwersja daty na tekst. W tym artykule dowiesz się, dlaczego jest to ważne oraz jak to zrobić w języku Elm.

## Jak to zrobić
Konwersja daty na tekst może wydawać się trudna, ale w rzeczywistości w języku Elm jest to bardzo proste. Oto przykładowy kod, który pokaże Ci, jak to zrobić:

```
Elm
import Time

dateToString : Time.Posix -> String
dateToString date =
    let
        millis = date * 1000
    in
    case Time.fromMillis millis of
        Err _ ->
            ""

        Ok time ->
            Time.format "%d/%m/%Y" time
```

W powyższym przykładzie wykorzystujemy moduł Time, aby przekonwertować polecsaną datę na milisekundy. Następnie wykorzystujemy funkcję `format` do przekonwertowania milisekund na format daty określony przez nas. W przypadku błędu, zwracamy pusty napis. Poniżej znajduje się przykładowe wyjście dla daty 20 marca 2021 roku:

```
20/03/2021
```

## Głębsze zagadnienia
Konwersja daty na tekst może być trochę bardziej złożona w przypadku, gdy chcemy wykorzystać różne formaty daty w zależności od języka lub preferencji użytkownika. W takim przypadku musimy wykorzystać biblioteki zewnętrzne, takie jak `elm-community/intl-extra`. Istnieje również możliwość wykorzystania niestandardowych formatów daty bez konieczności dodatkowych bibliotek.

## Zobacz także
Jeśli chcesz dowiedzieć się więcej o konwersji daty w języku Elm, polecamy zapoznać się z następującymi materiałami:

- Oficjalna dokumentacja języka Elm, zawierająca przykłady i wyjaśnienia na temat konwersji daty: https://elm-lang.org/docs/format
- Artykuł na stronie małego startupu, w którym omawiane są niestandardowe formaty daty: https://www.oplop.net/date-formatting-in-elm.html
- Repozytorium biblioteki `elm-community/intl-extra` na GitHubie: https://github.com/elm-community/intl-extra.