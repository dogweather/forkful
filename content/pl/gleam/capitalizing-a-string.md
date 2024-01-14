---
title:    "Gleam: Zmiana na wielkie litery ciągu znaków"
keywords: ["Gleam"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pl/gleam/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Dlaczego

Chcesz nauczyć się większego wykorzystywania języka Gleam? A może po prostu potrzebujesz sposobu na przetwarzanie tekstów w swoim kodzie? Niezależnie od powodu, kapitalizowanie stringów jest przydatną funkcją, którą warto poznać.

## Jak to zrobić

Kapitalizowanie stringów w języku Gleam jest bardzo proste. Wystarczy użyć wbudowanej funckji `String.capitalize()`! Poniżej znajdziesz przykładowy kod oraz oczekiwany wynik.

```Gleam
import String

let str = "witaj świecie"
let result = String.capitalize(str)

## Witaj świecie
```

Jest to bardzo przydatna funkcja, szczególnie przy przetwarzaniu danych wejściowych od użytkownika lub generowaniu wyjścia w odpowiednim formacie.

## Głębsze zanurzenie

Kapitalizowanie stringów w języku Gleam opiera się na funkcji `String.to_title_case()`, która jest częścią biblioteki standardowej języka. Funkcja ta przetwarza podany string i zwraca kopię z wszystkimi wyrazami zaczynającymi się wielką literą, a pozostałe litery małymi. Jest to wygodny i niezawodny sposób na zmianę formatu tekstu.

Poza tym, istnieje również funkcja `String.to_upper_case()` i `String.to_lower_case()`, które pozwalają na konwersję całego stringa do wielkich lub małych liter, odpowiednio.

## Zobacz też

- [Dokumentacja Gleam - String](https://gleam.run/documentation/standard-libraries/#string)
- [Oficjalna strona języka Gleam](https://gleam.run/)
- [Przewodnik po języku Gleam](https://gleam.run/book/introduction.html)