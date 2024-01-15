---
title:                "Zmiana na wielką literę ciągu znaków"
html_title:           "Elm: Zmiana na wielką literę ciągu znaków"
simple_title:         "Zmiana na wielką literę ciągu znaków"
programming_language: "Elm"
category:             "Elm"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/elm/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Dlaczego

Czy kiedykolwiek zastanawiałeś się, dlaczego w niektórych sytuacjach potrzebujemy zmieniać wielkość liter w tekście? Może jesteśmy przyzwyczajeni do pisania wyrazów z dużych liter na początku zdania lub chcemy podkreślić ważne słowo? W programowaniu również często wykorzystuje się zmianę wielkości liter, na przykład przy tworzeniu nazw zmiennych lub funkcji. Dlatego warto poznać prosty sposób na wykonanie tej operacji w języku Elm.

## Jak to zrobić?

W Elm istnieje funkcja o nazwie `String.toUpper`, która zamienia wszystkie litery w danym stringu na wielkie. Przykładowy kod wygląda następująco:

```
Elm String.toUpper "witaj świecie"
```

Wynik działania powyższego kodu to `"WITAJ ŚWIECIE"`. Proste, prawda? Ale co z jednostkami diakrytycznymi, takimi jak polskie znaki, które również powinny być zamieniane na duże litery? W takim przypadku wykorzystamy funkcję `String.toUpperLocale`, która uwzględnia różnice między alfabetami. Oto przykład:

```
Elm String.toUpperLocale "zażółć gęślą jaźń"
```

Ten kod zwróci `"ZAŻÓŁĆ GĘŚLĄ JAŹŃ"`.

Jednak zwykła zmiana wszystkich liter na duże nie zawsze jest wymagana. Może chcesz zamienić tylko pierwszą literę na wielką, jak w przypadku tytułów. Do tego celu użyjemy funkcji `String.capitalize`, przykładowo:

```
Elm String.capitalize "artykuł o zmianie wielkości liter w Elm"
```

Ten kod zwróci `"Artykuł o zmianie wielkości liter w Elm"`.

## Głębsza analiza

Funkcje `String.toUpper`, `String.toUpperLocale` i `String.capitalize` są dostępne w module `String` i są częścią standardowej biblioteki języka Elm. Są one bardzo proste w użyciu i mogą znacznie ułatwić pracę przy manipulacji tekstem.

Warto też wspomnieć, że w Elm nie ma możliwości zmiany wielkości liter w istniejącym stringu; zawsze musisz zwrócić nowy string.

## Zobacz także
- Dokumentacja Elm: [String module](https://package.elm-lang.org/packages/elm/core/latest/String)
- Inne przykłady użycia funkcji w Module String: [Flipping letters](https://elmprogramming.com/flipping-letters-strings.html)
- Teoria wielkich literek w języku polskim: [Wikipedia](https://pl.wikipedia.org/wiki/Wielkie_literki_w_języku_polskim)